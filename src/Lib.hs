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
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.Process as P
import qualified Text.PDF.Info as PDFI


data FileInfo = FileInfo
    { _fileName :: FilePath
    , _modTime  :: UTCTime
    }


listFiles :: FilePath -> IO [FileInfo]
listFiles path = do
    dirExists <- Dir.doesDirectoryExist path
    if dirExists then do
        files <- Dir.getDirectoryContents path
        fileInfos <- mapM getFileInfo files
        pure $ filter isPdf fileInfos
    else pure []


sortFileInfoByDate :: [FileInfo] -> [FileInfo]
sortFileInfoByDate fileInfos =
    reverse $ sortWith _modTime fileInfos


getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = do
    modTime <- Dir.getModificationTime path
    pure $ FileInfo path modTime


isPdf :: FileInfo -> Bool
isPdf fileInfo =
    FilePath.takeExtension (_fileName fileInfo) == ".pdf"


-- Getting Filename suggestions:

fileNameSuggestions :: FilePath -> IO (Text, [Text])
fileNameSuggestions file = do
    pdfInfo <- PDFI.pdfInfo file

    topLines <- getTopLines file

    let
        baseName =
            FilePath.takeBaseName (FilePath.takeBaseName file)
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


getTopLines :: FilePath -> IO [Text]
getTopLines file = do
    plainTextContent <-
        E.try (P.readProcess "pdftotext" [file, "-", "-f", "1", "-l", "4"] "")
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


fileFile :: Config -> Text -> FilePath -> IO ()
fileFile conf newFileNameWithSpaces file = do
    Dir.createDirectoryIfMissing True (conf ^. Config.libraryDir)
    let newFileName = T.unpack $ finalFileName conf newFileNameWithSpaces
        newFile = newFileName <> ".pdf"
        newFilePath =
            (conf ^. Config.libraryDir) </> newFile

    -- _ <- P.createProcess
    --     ( P.proc "ls" ["-la"] )
    --     { P.std_out = P.NoStream, P.std_err = P.NoStream }

    case conf ^. Config.importAction of
        Config.Copy -> Dir.copyFile file newFilePath
        Config.Move -> Dir.renameFile file newFilePath
    -- pure ()


openFile :: FilePath -> IO ()
openFile file = do
    tryOpenWithMany ["xdg-open", "open"] file


tryOpenWithMany :: [String] -> FilePath -> IO ()
tryOpenWithMany [] _ = pure ()
tryOpenWithMany (e:es) file = do
    exe <- Dir.findExecutable e
    case exe of
        Nothing -> tryOpenWithMany es file
        Just _  -> tryOpenWith e file


tryOpenWith :: FilePath -> FilePath -> IO ()
tryOpenWith exePath file = do
    _ <- P.createProcess
        ( P.proc exePath [file] )
        { P.std_out = P.NoStream, P.std_err = P.NoStream }
    pure ()
