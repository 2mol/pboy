{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( FileInfo(..)
    , finalFileName
    , listFiles
    , fileFile
    , fileNameSuggestions
    , openFile
    , sortFileInfoByDate
    ) where

import           Config (Config)
import qualified Config
import           Control.Exception as E
import qualified Data.Char as C
import qualified Data.Either.Combinators as Either
import           Data.Function ((&))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Titlecase (titlecase)
import           Data.Time.Clock (UTCTime)
import           GHC.Exts (sortWith)
import           Lens.Micro ((^.))
import           Path (Abs, Dir, File, Path, (</>))
import qualified Path
import qualified Path.IO as Path
import qualified System.FilePath as F
import qualified System.Process as P
import qualified GHC.IO.Handle.Types as IOHT
import           System.Directory (findExecutable)
import qualified Text.PDF.Info as PDFI

data FileInfo = FileInfo
    { _fileName :: Path Abs File
    , _modTime  :: UTCTime
    }


listFiles :: Path Abs Dir -> IO [FileInfo]
listFiles path = do
    dirExists <- Path.doesDirExist path
    if dirExists then do
        files <- snd <$> Path.listDir path
        fileInfos <- mapM getFileInfo files
        pure $ filter isPdf fileInfos
    else pure []


sortFileInfoByDate :: [FileInfo] -> [FileInfo]
sortFileInfoByDate fileInfos =
    reverse $ sortWith _modTime fileInfos


getFileInfo :: Path Abs File -> IO FileInfo
getFileInfo path = do
    modTime <- Path.getModificationTime path
    pure $ FileInfo path modTime


isPdf :: FileInfo -> Bool
isPdf fileInfo =
    Path.fileExtension (_fileName fileInfo) == ".pdf"


-- Getting Filename suggestions:

fileNameSuggestions :: Path Abs File -> IO (Text, [Text])
fileNameSuggestions file = do
    pdfInfo <- PDFI.pdfInfo $ Path.fromAbsFile file

    topLines <- getTopLines file

    let
        baseName =
            F.takeBaseName (Path.fromRelFile $ Path.filename file)
                & T.pack
                & T.replace "_" " "

        maybeCleanFileName =
            baseName
                & sanitize
                & boolToMaybe lengthCheck

        maybeTitle =
            Either.rightToMaybe pdfInfo
                >>= PDFI.pdfInfoTitle
                & fmap sanitize
                >>= boolToMaybe lengthCheck

        suggestions =
            maybeCleanFileName : maybeTitle : fmap Just topLines
                & Maybe.catMaybes
                & List.nub
                & take 5

    pure (baseName, suggestions)


getTopLines :: Path Abs File -> IO [Text]
getTopLines file = do
    plainTextContent <-
        E.try (P.readProcess "pdftotext" [Path.fromAbsFile file, "-", "-f", "1", "-l", "4"] "")
        :: IO (Either SomeException String)
    let
        topLines =
            case plainTextContent of
                Left _ -> []
                Right content ->
                    T.pack content
                        & T.lines
                        & take 16 -- totally arbitrary. subject to improvement later
                        & fmap sanitize
                        & filter lengthCheck
    pure topLines


lengthCheck :: Text -> Bool
lengthCheck t = T.length t >= 3 && T.length t <= 64


boolToMaybe :: (a -> Bool) -> a -> Maybe a
boolToMaybe check a =
    if check a
        then Just a
        else Nothing


-- 1. remove double spaces / double underscores
-- 2. strip out all except ascii, alphanumeric, spaces, underscores, dashes
-- 3. display with spaces in UI, replace with '_' for filenames later

sanitize :: Text -> Text
sanitize text =
    text
        & T.replace "_" " "
        & T.unwords . T.words
        & T.unpack
        & filter validChars
        & titlecase
        & T.pack


validChars :: Char -> Bool
validChars x =
    case x of
        '_' -> True
        '-' -> True
        _   -> C.isLetter x || C.isSpace x


-- shelving files into library folder

finalFileName :: Text -> Text
finalFileName text =
    text
        & T.unwords . T.words
        & T.replace " " "_"


fileFile :: Config -> Text -> Path Abs File -> IO ()
fileFile conf newFileName file = do
    _ <- Path.ensureDir (conf ^. Config.libraryDir)
    newFile <- Path.parseRelFile (T.unpack newFileName <> Path.fileExtension file)
    let
        newFilePath =
            conf ^. Config.libraryDir </> newFile

    case conf ^. Config.importAction of
        Config.Copy -> Path.copyFile file newFilePath
        Config.Move -> Path.renameFile file newFilePath


openFile :: Path Abs File -> IO ()
openFile file = do
    xdgOpenPresent <- findExecutable "xdg-open"
    if Maybe.isJust xdgOpenPresent
        then do
            tryOpenWith file "xdg-open"
            pure ()
        else do
            openPresent <- findExecutable "open"
            if Maybe.isJust openPresent
                then do
                    tryOpenWith file "open"
                    pure ()
                else pure ()


tryOpenWith :: Path Abs File -> FilePath -> IO ()
tryOpenWith file cmd = do
    _ <- P.createProcess (P.proc cmd [Path.fromAbsFile file]){ P.std_out = P.NoStream, P.std_err = P.NoStream }
    return ()
