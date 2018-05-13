{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.Semigroup   ((<>))
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified System.Directory as D
import           System.FilePath  ((</>))
import qualified System.FilePath  as F


-- want:
-- 0. list files available in Download/Inbox
-- 0. read library path from config
-- 1. move file from download to library folder
-- 2. rename file while moving

inboxFiles :: FilePath -> IO [FilePath]
inboxFiles inboxPath = do
    -- homeDir <- D.getHomeDirectory
    files <- D.getDirectoryContents inboxPath --(homeDir </> inboxFolder)
    pure $ filter isPdf files

isPdf :: FilePath -> Bool
isPdf f = F.takeExtension f == ".pdf"

-- data FileInfo = FileInfo
--     { fiPath :: FilePath
--     , fiName :: Text }

safeMove :: FilePath -> FilePath -> IO ()
safeMove origPath destPath = do
    D.createDirectoryIfMissing True (F.takeDirectory destPath)
    -- TODO: make sure the permissions are sane?
    -- TODO: catch exceptions in here?
    D.renameFile origPath destPath

--


fileFile :: FilePath -> IO ()
fileFile file = undefined



specifyFileName :: FilePath -> IO FilePath
specifyFileName = undefined
