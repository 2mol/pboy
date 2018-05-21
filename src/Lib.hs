module Lib where

import           Config                  (Config)
import qualified Config
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
import qualified System.Directory as D
import           System.FilePath  ((<.>), (</>))
import qualified System.FilePath  as F
import qualified System.Process   as P
import qualified Text.PDF.Info    as PDFI

constSupportedExtensions :: Set String
constSupportedExtensions = S.fromList [".pdf"]

getDefaultConfig :: IO Config
getDefaultConfig = do
    homeDir <- D.getHomeDirectory
    pure $ Config.testDefaultConfig homeDir

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
    plainTextContent <- P.readProcess "pdftotext" [fullFilePath, "-"] ""

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
            T.pack plainTextContent
                & T.lines
                & take 16 -- totally arbitrary. subject to improvement later
                & fmap sanitize
                & filter lengthCheck
                & fmap Just

        suggestions =
            cleanFileName : maybeTitle : topContent
                & Maybe.catMaybes

    pure $ baseName :| take 4 (List.nub suggestions)

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

fileFile :: Config -> FilePath -> Text -> IO ()
fileFile conf origFileName newFileName = do
    let
        newFilePath =
            conf ^. Config.libraryDir </> (T.unpack newFileName) <.> "pdf"

        origFilePath =
            (conf ^. Config.inboxDir) </> (F.takeFileName origFileName)

    D.copyFile origFilePath newFilePath
