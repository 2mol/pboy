module Lib where

import qualified Data.Char               as C
import           Data.Either.Combinators (rightToMaybe)
import qualified Data.List               as List
import qualified Data.Maybe              as Maybe
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Text.Titlecase     (titlecase)
import           Data.Time.Clock         (UTCTime)
import           GHC.Exts                (sortWith)
import qualified System.Directory        as D
import           System.FilePath         ((<.>), (</>))
import qualified System.FilePath         as F
import qualified System.Process          as P
import qualified Text.PDF.Info           as PDFI


-- want:
-- 0. list files available in Download/Inbox
-- 0. read library path from config
-- 1. move file from download to library folder
-- 2. rename file while moving

infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

constSupportedExtensions :: Set String
constSupportedExtensions = S.fromList [".pdf"]

data Config = Config
    { inboxDir   :: FilePath
    , libraryDir :: FilePath
    } deriving (Show)

getDefaultConfig :: IO Config
getDefaultConfig = do
    homeDir <- D.getHomeDirectory
    let inbox = (homeDir </> "Downloads")
        library = (homeDir </> "pboy-test")
    pure $ Config inbox library

data FileInfo = FileInfo
    { _fileName :: FilePath
    , _modTime  :: UTCTime
    }

listFiles :: FilePath -> IO [FileInfo]
listFiles path = do
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

fileFile :: Config -> FilePath -> Text -> IO ()
fileFile Config{libraryDir} origFilePath newFileName = do
    let newFilePath = libraryDir </> (T.unpack newFileName) <.> "pdf"
    D.createDirectoryIfMissing True libraryDir
    D.copyFile origFilePath newFilePath

fileNameSuggestions :: Config -> FilePath -> IO [Text]
fileNameSuggestions Config{inboxDir} filePath = do
    let
        fileName = F.takeFileName filePath
        fullFilePath = inboxDir </> fileName
    plainTextContent <- P.readProcess "pdftotext" [fullFilePath, "-"] ""

    pdfInfo <- PDFI.pdfInfo fullFilePath

    let
        fileNameText =
            F.takeBaseName fileName
                |> T.pack
                |> sanitize
                |> Just

        maybeTitle =
            rightToMaybe pdfInfo
                >>= PDFI.pdfInfoTitle
                |> fmap sanitize
                >>= boolToMaybe lengthCheck

        topContent =
            T.pack plainTextContent
                |> T.lines
                |> take 16 -- totally arbitrary. subject to improvement later
                |> fmap sanitize
                |> filter lengthCheck
                |> fmap Just

        suggestions =
            fileNameText : maybeTitle : topContent
                |> Maybe.catMaybes

    pure $ take 4 $ List.nub suggestions

-- TODO: create or use a filename sanitization function
-- sanitize :: Text -> Maybe Text
-- sanitize t = Just t

-- 1. strip out all except ascii, alphanumeric, spaces, underscores, dashes
-- 2. remove double spaces / underscores

lengthCheck :: Text -> Bool
lengthCheck t = T.length t >= 3 && T.length t <= 64

boolToMaybe :: (a -> Bool) -> a -> Maybe a
boolToMaybe check a =
    if check a
        then Just a
        else Nothing

sanitize :: Text -> Text
sanitize t =
    let
        makeBetter str =
            str
                |> filter validChars
                -- |> replace underscores
                -- |> remove double spaces etc
                |> titlecase

    in (T.pack . makeBetter . T.unpack) t

validChars :: Char -> Bool
validChars x =
    case x of
        '_' -> True
        _   -> C.isLetter x || C.isSpace x
