{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Config                  (Config)
import qualified Config
import           Control.Exception       as E
import qualified Data.Char               as C
import           Data.Either.Combinators (rightToMaybe)
import           Data.Function           ((&))
import qualified Data.List               as List
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.Maybe              as Maybe
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Text.Titlecase     (titlecase)
import           Data.Time.Clock         (UTCTime)
import           GHC.Exts                (sortWith)
import           Lens.Micro              ((^.))
import           Path                    (Abs, Dir, File, Path, (</>))
import qualified Path
import qualified Path.IO                 as Path
import qualified System.FilePath         as F
import qualified System.Process          as P
import qualified Text.PDF.Info           as PDFI


data FileInfo = FileInfo
    { _fileName :: Path Abs File
    , _modTime  :: UTCTime
    }


listFiles :: Path Abs Dir -> IO [FileInfo]
listFiles path = do
    Path.ensureDir path
    files <- snd <$> Path.listDir path
    fileInfos <- mapM getFileInfo files
    let sortedFileInfos = reverse $ sortWith _modTime fileInfos
    pure $ filter isPdf sortedFileInfos


getFileInfo :: Path Abs File -> IO FileInfo
getFileInfo path = do
    modTime <- Path.getModificationTime path
    pure $ FileInfo path modTime


isPdf :: FileInfo -> Bool
isPdf fileInfo =
    Path.fileExtension (_fileName fileInfo) == ".pdf"


-- Getting Filename suggestions:

fileNameSuggestions :: Path Abs File -> IO (NonEmpty Text)
fileNameSuggestions fullFilePath = do
    plainTextContent <-
        P.readProcess "pdftotext" [Path.fromAbsFile fullFilePath, "-"] ""
            & tryJust displayErr

    pdfInfo <- PDFI.pdfInfo $ Path.fromAbsFile fullFilePath

    let
        baseName =
            F.takeBaseName (Path.fromRelFile $ Path.filename fullFilePath)
                & T.pack
                & T.replace "_" " "

        cleanFileName =
            baseName
                & sanitize
                & boolToMaybe lengthCheck

        maybeTitle =
            rightToMaybe pdfInfo
                >>= PDFI.pdfInfoTitle
                & fmap sanitize
                >>= boolToMaybe lengthCheck

        topContent =
            case plainTextContent of
                Left _ -> []
                Right content ->
                    T.pack content
                        & T.lines
                        & take 16 -- totally arbitrary. subject to improvement later
                        & fmap sanitize
                        & filter lengthCheck
                        & fmap Just

        suggestions =
            cleanFileName : maybeTitle : topContent
                & Maybe.catMaybes

    pure $ baseName :| take 5 (List.nub suggestions)


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


fileFile :: Config -> Path Abs File -> Text -> IO ()
fileFile conf origFilePath newFileName = do
    newFile <- Path.parseRelFile (T.unpack newFileName ++ Path.fileExtension origFilePath)
    let
        newFilePath =
            conf ^. Config.libraryDir </> newFile

    case conf ^. Config.importAction of
        Config.Copy -> Path.copyFile origFilePath newFilePath
        Config.Move -> Path.renameFile origFilePath newFilePath


openFile :: Path Abs File -> IO (Either String ())
openFile filePath = do
    linuxOpen <-
        P.readProcess "xdg-open" [Path.fromAbsFile filePath] ""
            & tryJust displayErr

    case linuxOpen of
        Left _ ->
            do
                _ <-
                    P.readProcess "open" [Path.fromAbsFile filePath] ""
                        & tryJust displayErr
                pure $ Right ()
        Right _ ->
            pure $ Right ()


displayErr :: SomeException -> Maybe String
displayErr e =
    Just $ displayException e
