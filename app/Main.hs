
module Main where

import Snap
import Snap.Http.Server (httpServe, defaultConfig, setPort)
import Snap.Core (route)
import Snap.Util.FileServe (serveDirectory)
import qualified Data.ByteString.Char8 as B
import System.Directory (listDirectory)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = httpServe (setPort 8111 defaultConfig) site

site :: Snap ()
site = route [( (B.pack "list"), listFilesHandler)
               , (B.pack "", serveDirectory ".")]
 
 -- Handler to list files in the current directory
listFilesHandler :: Snap ()
listFilesHandler = do
    files <- liftIO (listDirectory ".")
    let fileList =  B.pack $ unlines files
    writeBS fileList

