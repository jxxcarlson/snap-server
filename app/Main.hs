
module Main where

import Snap
import Snap.Http.Server (httpServe, defaultConfig, setPort)
import Snap.Core (route, modifyResponse, setContentType)
import Snap.Util.FileServe (serveDirectory, serveFile)
import qualified Data.ByteString.Char8 as B
import System.FilePath (takeExtension, takeFileName, (</>))
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import Text.Read (readMaybe)

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

site :: String -> Snap ()
site serverDirectory = route [ (B.pack "", fileAndDirectoryHandler) ]
 

fileAndDirectoryHandler :: Snap ()
fileAndDirectoryHandler = do
    rq <- getRequest
    let dirPath = "." </> B.unpack (rqPathInfo rq)
    isDir <- liftIO $ doesDirectoryExist dirPath
    isFile <- liftIO $ doesFileExist dirPath
    if isDir
        then do
            contents <- liftIO $ listDirectory dirPath
            let links = map (makeLink dirPath) contents
            let html = B.pack $ "<html><body>" ++ unlines links ++ "</body></html>"
            modifyResponse $ setContentType $ B.pack "text/html; charset=utf-8"
            writeBS html
    else if isFile 
        then do
            serveResource dirPath
    else
            writeBS $ B.pack "Not a directory or directory does not exist."

makeLink :: FilePath -> String -> String
makeLink dirPath file = "<a href='" ++ dirPath ++ "/" ++ file ++ "'>" ++ file ++ "</a><br>"


serveAsText :: FilePath -> Snap ()
serveAsText filePath = do
    fileContents <- liftIO $ B.readFile filePath
    modifyResponse $ setContentType $ B.pack "text/plain; charset=utf-8"
    writeBS fileContents

serveAsPDF :: FilePath -> Snap ()
serveAsPDF filePath = do
    fileContents <- liftIO $ B.readFile filePath
    modifyResponse $ setContentType $ B.pack "application/pdf"
    writeBS fileContents

serveResource :: FilePath -> Snap ()
serveResource filePath = do
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
        ".pdf"  -> serveAsPDF filePath
        _       -> serveAsText filePath


serveWithMimeType :: B.ByteString -> FilePath -> Snap ()
serveWithMimeType mimeType filePath = do
    modifyResponse $ setContentType mimeType
    serveFile filePath