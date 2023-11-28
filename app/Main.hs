
module Main where

import Snap
import Snap.Http.Server (httpServe, defaultConfig, setPort)
import Snap.Core (route, modifyResponse, setContentType)
import Snap.Util.FileServe (serveDirectory, serveFile)
import qualified Data.ByteString.Char8 as B
import System.FilePath (takeExtension)
import System.Directory (listDirectory, doesFileExist)
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
site serverDirectory = route [(B.pack "list", (listFilesHandler serverDirectory))
               , (B.pack "", fileHandler)]
 
--- Handler to list files in the current directory as clickable links
listFilesHandler :: String -> Snap ()
listFilesHandler serverDirectory = do
    files <- liftIO (listDirectory serverDirectory)
    let links = map makeLink files
    let html = B.pack $ "<html><body>" ++ unlines links ++ "</body></html>"
    modifyResponse $ setContentType  $ B.pack "text/html; charset=utf-8"
    writeBS html

-- Function to create an HTML link for a file that opens in a new tab/window
makeLink :: String -> String
makeLink file = "<a href='/" ++ file ++ "'>" ++ file ++ "</a><br>"


fileHandler :: Snap ()
fileHandler = do
    rq <- getRequest
    let uri = rqURI rq
    let filePath = "." ++ B.unpack uri  -- Constructing the file path from URI
    exists <- liftIO $ doesFileExist filePath
    if exists then
        serveImage filePath
    else
        pass  -- Fall back to default behavior

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

serveImage :: FilePath -> Snap ()
serveImage filePath = do
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
        -- _       -> pass

serveWithMimeType :: B.ByteString -> FilePath -> Snap ()
serveWithMimeType mimeType filePath = do
    modifyResponse $ setContentType mimeType
    serveFile filePath