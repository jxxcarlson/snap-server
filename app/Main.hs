{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Snap as S
import Snap.Http.Server (httpServe, defaultConfig, setPort)
import Snap.Core (route, modifyResponse, setContentType)
import Snap.Util.FileServe (serveDirectory, serveFile)
import Snap.Util.CORS (CORSOptions(..), HashableMethod(..), OriginList(Origins), applyCORS, mkOriginSet)
import qualified Data.ByteString.Char8 as B
import System.FilePath (takeExtension, takeFileName, (</>))
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Data.CaseInsensitive (mk)
import qualified Data.HashSet as HashSet
import Network.URI (parseURI)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T




allowedOrigins :: [String]
allowedOrigins =
  [ "http://localhost:8000", "https://scripta.io" ]


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
site serverDirectory = route
            [ (B.pack "", S.method S.OPTIONS handleOptions <|> fileAndDirectoryHandler)
           ,  (B.pack "postdata", S.method S.OPTIONS handleOptions <|> S.method S.POST handlePost)]
  

handleOptions :: S.Snap ()
handleOptions = do
    liftIO (putStrLn "Entering handleOptions")
    setCorsHeaders
    modifyResponse $ S.setResponseCode 200
    S.writeBS $ B.pack "OPTIONS: handled"

fileAndDirectoryHandler :: S.Snap ()
fileAndDirectoryHandler = 
    allow S.GET allowedOrigins $ do
    rq <- S.getRequest
    let dirPath = "." </> B.unpack (S.rqPathInfo rq)
    S.writeBS $ S.rqPathInfo rq
    liftIO (putStrLn $ show $ dirPath)
    isFile <- liftIO $ doesFileExist dirPath
    isDir <- liftIO $ doesDirectoryExist dirPath
    if isDir
        then do
            contents <- liftIO $ listDirectory dirPath
            let links = map (makeLink dirPath) contents
            let html = B.pack $ "<html><body>" ++ unlines links ++ "</body></html>"
            modifyResponse $ setContentType $ B.pack "text/html; charset=utf-8"
            S.writeBS html
            liftIO (putStrLn "Directory read successfully")
    else if isFile 
        then do
            serveResource dirPath
            liftIO (putStrLn "File served successfully")
    else
            S.writeBS $ B.pack "Not a directory or directory does not exist."

makeLink :: FilePath -> String -> String
makeLink dirPath file = "<a href='" ++ dirPath ++ "/" ++ file ++ "'>" ++ file ++ "</a><br>"


serveAsText :: FilePath -> S.Snap ()
serveAsText filePath = do
    liftIO (putStrLn "serving TEXT")
    fileContents <- liftIO $ B.readFile filePath
    liftIO $ putStrLn $ "Data served from: " ++ filePath
    liftIO $ putStrLn $ "Data contnets: " ++ B.unpack fileContents
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


-- CORS

setCorsHeaders :: S.Snap ()
setCorsHeaders = modifyResponse $ do
    S.addHeader (mk $ B.pack "Access-Control-Allow-Origin") (B.pack "*")
    S.addHeader (mk $ B.pack "Access-Control-Allow-Methods") (B.pack "GET, POST, OPTIONS")
    S.addHeader (mk $ B.pack "Access-Control-Allow-Headers") (B.pack "Origin, Accept, Content-Type")



allow :: S.Method -> [String] -> S.Snap () -> S.Snap ()
allow method_ origins snap = 
  applyCORS (toOptions method_ origins) $ S.method method_ snap

toOptions :: (Monad m) => S.Method -> [String] -> CORSOptions m
toOptions method_ origins =
  let
    allowedOrigins = toOriginList origins
    allowedMethods = HashSet.singleton (HashableMethod method_)
  in
  CORSOptions
    { corsAllowOrigin = return allowedOrigins
    , corsAllowCredentials = return True
    , corsExposeHeaders = return HashSet.empty
    , corsAllowedMethods = return allowedMethods
    , corsAllowedHeaders = return
    }


toOriginList :: [String] -> OriginList
toOriginList origins =
  Origins $ mkOriginSet $
    case traverse parseURI origins of
      Just uris -> uris
      Nothing -> error "invalid entry given to toOriginList list"

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
    allow S.POST allowedOrigins $ do
    setCorsHeaders
    modifyResponse $ S.setHeader "Content-Type" "application/json"
    body <- S.readRequestBody 1000000 -- Max size of the request body
    case Aeson.decode body of
        Just postData -> do
            liftIO $ writeFile (path postData) (content postData)
            S.writeBS   "Data written successfully"
            liftIO (putStrLn "Data written successfully")
        Nothing -> do
            modifyResponse $ S.setResponseCode 400
            -- S.writeBS "Invalid JSON data"
            liftIO (putStrLn "Invalid JSON data")
