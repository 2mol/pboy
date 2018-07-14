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
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Text.Titlecase     (titlecase)
import           Data.Time.Clock         (UTCTime)
import           GHC.Exts                (sortWith)
import           Lens.Micro              ((^.))
import qualified System.Directory        as D
import           System.FilePath         ((<.>), (</>))
import qualified System.FilePath         as F
import qualified System.Process          as P
import qualified Text.PDF.Info           as PDFI


constSupportedExtensions :: Set String
constSupportedExtensions = S.fromList [".pdf"]


data FileInfo = FileInfo
    { _fileName :: FilePath
    , _modTime  :: UTCTime
    }


listFiles :: FilePath -> IO [FileInfo]
listFiles path = do
    D.createDirectoryIfMissing True path
    files <- D.listDirectory path
    fileInfos <- mapM getFileInfo (fmap (\f -> path </> f) files)
    let sortedFileInfos = reverse $ sortWith _modTime fileInfos
    pure $ filter fileSupported sortedFileInfos


getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = do
    modTime <- D.getModificationTime path
    pure $ FileInfo (F.takeFileName path) modTime


fileSupported :: FileInfo -> Bool
fileSupported fileInfo =
    let extension = F.takeExtension $ _fileName fileInfo
    in S.member extension constSupportedExtensions


-- Getting Filename suggestions:

fileNameSuggestions :: Config -> FilePath -> IO (NonEmpty Text)
fileNameSuggestions config filePath = do
    let
        fileName = F.takeFileName filePath
        fullFilePath = (config ^. Config.inboxDir) </> fileName

    plainTextContent <-
        P.readProcess "pdftotext" [fullFilePath, "-"] ""
            & tryJust displayErr

    pdfInfo <- PDFI.pdfInfo fullFilePath

    let
        baseName =
            F.takeBaseName fileName
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


fileFile :: Config -> FilePath -> Text -> IO ()
fileFile conf origFileName newFileName = do
    let
        newFilePath =
            conf ^. Config.libraryDir </> (T.unpack newFileName) <.> "pdf"

        origFilePath =
            (conf ^. Config.inboxDir) </> (F.takeFileName origFileName)

    case conf ^. Config.importAction of
        Config.Copy -> D.copyFile origFilePath newFilePath
        Config.Move -> D.renameFile origFilePath newFilePath


openFile :: Config -> FilePath -> IO (Either String ())
openFile conf fileName = do
    let
        cleanFileName =
            F.takeFileName fileName

        filePath =
            conf ^. Config.libraryDir </> cleanFileName

    linuxOpen <-
        P.readProcess "xdg-open" [filePath] ""
            & tryJust displayErr

    case linuxOpen of
        Left _ ->
            do
                _ <-
                    P.readProcess "open" [filePath] ""
                        & tryJust displayErr
                pure $ Right ()
        Right _ ->
            pure $ Right ()


displayErr :: SomeException -> Maybe String
displayErr e =
    Just $ displayException e
