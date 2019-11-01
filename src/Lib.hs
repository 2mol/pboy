{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( FileInfo(..)
    , finalFileName
    , listFiles
    , fileFile
    , fileNameSuggestions
    , openFile
    , sortFileInfoByDate
    , sortFileInfoByName
    , sortFileInfo
    , SortType(..)
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
import qualified System.Directory
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


data SortType = SortDate | SortName


sortFileInfoByDate :: [FileInfo] -> [FileInfo]
sortFileInfoByDate fileInfos =
    reverse $ sortWith _modTime fileInfos


sortFileInfoByName :: [FileInfo] -> [FileInfo]
sortFileInfoByName fileInfos =
    reverse $ sortWith _fileName fileInfos


sortFileInfo :: SortType -> [FileInfo] -> [FileInfo]
sortFileInfo SortDate = sortFileInfoByDate
sortFileInfo SortName = sortFileInfoByName


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
lengthCheck t =
    -- maximum filename length on most Unix FSs (255) - extension (.pdf, 4) = 251
    T.length t >= 3 && T.length t <= 251


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

finalFileName :: Config -> Text -> Text
finalFileName conf text =
    text
        & T.unwords . T.words
        & T.replace " " (conf ^. Config.wordSeparator)


fileFile :: Config -> Text -> Path Abs File -> IO ()
fileFile conf newFileName file = do
    _ <- Path.ensureDir (conf ^. Config.libraryDir)
    newFile <- Path.parseRelFile (T.unpack newFileName <> Path.fileExtension file)
    let
        newFilePath =
            (conf ^. Config.libraryDir) </> newFile

    case conf ^. Config.importAction of
        Config.Copy -> Path.copyFile file newFilePath
        Config.Move -> Path.renameFile file newFilePath


openFile :: Path Abs File -> IO ()
openFile file = do
    tryOpenWithMany ["xdg-open", "open"] file


tryOpenWithMany :: [String] -> Path Abs File -> IO ()
tryOpenWithMany [] _ = pure ()
tryOpenWithMany (e:es) file = do
    exe <- System.Directory.findExecutable e
    case exe of
        Nothing -> tryOpenWithMany es file
        Just _  -> tryOpenWith e file


tryOpenWith :: FilePath -> Path Abs File -> IO ()
tryOpenWith exePath file = do
    P.createProcess
        ( P.proc exePath [Path.fromAbsFile file] )
        { P.std_out = P.NoStream, P.std_err = P.NoStream }
    pure ()
