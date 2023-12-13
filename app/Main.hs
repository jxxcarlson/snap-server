{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Snap as S
import Snap.Http.Server (httpServe, defaultConfig, setPort)
import Snap.Core (route, modifyResponse, setContentType)
import Snap.Util.FileServe (serveFile)
import Snap.Util.CORS (CORSOptions(..), HashableMethod(..), OriginList(Origins), applyCORS, mkOriginSet)
import qualified Data.ByteString.Char8 as B
import System.FilePath (takeExtension, (</>))
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Data.CaseInsensitive (mk)
import qualified Data.HashSet as HashSet
import Network.URI (parseURI)
import qualified Data.Aeson as Aeson
import qualified Data.Text ()
import qualified Cors
import System.IO (hFlush, stdout)

-- curl -X OPTIONS -H "Origin: http://localhost:8000" -H "Access-Control-Request-Method: POST" http://localhost:8080/postdata -i\n
-- curl -X OPTIONS -H "Origin: http://localhost:8000" -H "Access-Control-Request-Method: POST" http://localhost:8080/foo -i\n



allowedOrigins :: [String]
allowedOrigins =
  [ "http://localhost:8000", "https://scripta.io", "null" ]

main :: IO ()
main = do
         args <- getArgs
         case args of 
            (port_:serverDirectory:[]) -> 
                case readMaybe port_ of 
                    Nothing -> putStrLn "First argument (port) can't be converted to an integer"
                    Just port -> 
                        httpServe (setPort port defaultConfig) (site serverDirectory)
            _ -> putStrLn "command takes exactly two args, <port> and <server root directory>"



site :: String -> S.Snap ()
site serverDirectory = do
    liftIO $ putStrLn "Starting site function"
    route
        [ (B.pack "", logRoute "(get)" >> fileAndDirectoryHandler)
        , (B.pack "foo", logRoute "foo" >> (S.method S.OPTIONS handleOptions))
        , (B.pack "postdata", logRoute "postdata" >> (S.method S.OPTIONS handleOptions <|> S.method S.POST handlePost))
	]
    liftIO $ putStrLn "Finished site function\n"


logRoute :: String -> S.Snap ()
logRoute route = liftIO $ putStrLn $ "Accessing route: " ++ route


handleOptions :: S.Snap ()
handleOptions = do
    liftIO (putStrLn "Enter: handleOptions")
    liftIO (hFlush stdout)
    setCorsHeaders
    modifyResponse $ S.setResponseCode 200
    liftIO (putStrLn "Exit: handleOptions")
    liftIO (hFlush stdout)
    S.writeBS $ B.pack ""

setCorsHeaders :: S.Snap ()
setCorsHeaders = modifyResponse $ do
    S.addHeader (mk $ B.pack "Access-Control-Allow-Origin") (B.pack "*")
    S.addHeader (mk $ B.pack "Access-Control-Allow-Methods") (B.pack "GET, POST, OPTIONS")
    S.addHeader (mk $ B.pack "Access-Control-Allow-Headers") (B.pack "Origin, Accept, Content-Type")

fileAndDirectoryHandler :: S.Snap ()
fileAndDirectoryHandler =
    Cors.allow S.GET allowedOrigins $ do
    rq <- S.getRequest
    let rqPath = S.rqPathInfo rq
    liftIO (putStrLn $ "rqPathInfo: " ++ show rqPath)
    liftIO (hFlush stdout)
    if B.null rqPath
        then liftIO (putStrLn "Received empty path")
        else do
            let dirPath = "/var/www/dataserver/data" </> B.unpack rqPath

            liftIO (putStrLn $ "Directory path: " ++ dirPath)
            liftIO (hFlush stdout)
            isFile <- liftIO $ doesFileExist dirPath
	    liftIO (putStrLn $ "isFile: " ++ show isFile)
	    isDir <- liftIO $ doesDirectoryExist dirPath
            if isDir
              then do
                  contents <- liftIO $ listDirectory dirPath
                  let links = map (makeLink dirPath) contents
                  let html = B.pack $ "<html><body>" ++ unlines links ++ "</body></html>"
                  modifyResponse $ setContentType $ B.pack "text/html; charset=utf-8"
                  S.writeBS html
                  liftIO (putStrLn "Directory read successfully")
                  liftIO (hFlush stdout)
            else if isFile
                then do
                    serveResource dirPath
                    liftIO (putStrLn "File served successfully")
                    liftIO (hFlush stdout)
            else
                  S.writeBS $ B.pack "Not a directory or directory does not exist."



makeLink :: FilePath -> String -> String
makeLink dirPath file = "<a href='" ++ dirPath ++ "/" ++ file ++ "'>" ++ file ++ "</a><br>"

serveAsText :: FilePath -> S.Snap ()
serveAsText filePath = do
    fileContents <- liftIO $ B.readFile filePath
    modifyResponse $ setContentType $ B.pack "text/plain; charset=utf-8"
    S.writeBS fileContents

serveAsPDF :: FilePath -> S.Snap ()
serveAsPDF filePath = do
    fileContents <- liftIO $ B.readFile filePath
    modifyResponse $ setContentType $ B.pack "application/pdf"
    S.writeBS fileContents

serveResource :: FilePath -> S.Snap ()
serveResource filePath = do
    -- setCorsHeaders
    let ext = takeExtension filePath
    case ext of
        ".png"  -> serveWithMimeType (B.pack "image/png") filePath
        ".jpg"  -> serveWithMimeType (B.pack "image/jpeg") filePath
        ".jpeg" -> serveWithMimeType (B.pack "image/jpeg") filePath
        ".gif"  -> serveWithMimeType (B.pack "image/gif") filePath
        ".txt"  -> serveAsText filePath
        ".yaml" -> serveAsText filePath
        ".md"   -> serveAsText filePath
        ".hs"   -> serveAsText filePath
        ".elm"  -> serveAsText filePath
        ".csv"  -> serveAsText filePath
        ".pdf"  -> serveAsPDF filePath
        _       -> serveAsText filePath
    


serveWithMimeType :: B.ByteString -> FilePath -> S.Snap ()
serveWithMimeType mimeType filePath = do
    modifyResponse $ setContentType mimeType
    serveFile filePath

-- POST Data

data PostData = PostData
  { path :: FilePath,
    content :: String
  }

instance Aeson.FromJSON PostData where
  parseJSON = Aeson.withObject "PostData" $ \v -> PostData
    <$> v Aeson..: "path"
    <*> v Aeson..: "content"

handlePost :: S.Snap ()
handlePost =
  Cors.allow S.POST allowedOrigins $ do
    setCorsHeaders
    liftIO (putStrLn "Enter: handlePost")
    liftIO (hFlush stdout)
    body <- S.readRequestBody 1000000 -- Max size of the request body
    case Aeson.decode body of
        Just postData -> do
            liftIO (putStrLn $ "About to write data to " ++ show (path postData))
            liftIO (hFlush stdout)
            liftIO $ writeFile (path postData) (content postData)
            S.writeBS   "Data written successfully"
            liftIO (putStrLn "Data written successfully")
            liftIO (hFlush stdout)
        Nothing -> do
            modifyResponse $ S.setResponseCode 400
            S.writeBS "Invalid JSON data"
            liftIO (putStrLn "Invalid JSON data")
            liftIO (hFlush stdout)
