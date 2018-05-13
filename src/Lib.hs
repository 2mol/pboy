{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.Semigroup   ((<>))
import qualified System.Directory as Directory
import           System.FilePath  ((</>))
import qualified System.FilePath  as FilePath

-- want:
-- 0. list files available in Download/Inbox
-- 0. read library path from config
-- 1. move file from download to library folder
-- 2. rename file while moving

inboxPath :: FilePath
inboxPath = "/Users/juri/Downloads"

inboxFiles :: IO [FilePath]
inboxFiles = do
    files <- Directory.getDirectoryContents inboxPath
    pure $ filter isPdf files

isPdf :: FilePath -> Bool
isPdf f = FilePath.takeExtension f == ".pdf"

fileFile :: FilePath -> IO ()
fileFile = undefined



specifyFileName :: FilePath -> IO FilePath
specifyFileName = undefined
