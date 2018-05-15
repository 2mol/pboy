module Lib where

import           Data.Either.Combinators (rightToMaybe)
import qualified Data.Char as C
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Text               (Text)
import qualified Data.Text               as T
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

listFiles :: FilePath -> IO [FilePath]
listFiles path = do
    files <- D.getDirectoryContents path
    pure $ filter isSupported files

isSupported :: FilePath -> Bool
isSupported f = S.member (F.takeExtension f) constSupportedExtensions

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
                |> strip

        infoTitle =
            rightToMaybe pdfInfo
                >>= PDFI.pdfInfoTitle
                |> fmap strip
                >>= boolToMaybe lengthCheck

        topContent =
            T.pack plainTextContent
                |> T.lines
                |> fmap strip
                |> filter lengthCheck
                |> (take 4)

    pure $ case infoTitle of
                Just title  -> fileNameText : title : topContent
                Nothing -> fileNameText : topContent

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

strip :: Text -> Text
strip t = (T.pack . filter validChars . T.unpack) t

validChars :: Char -> Bool
validChars x =
    case x of
        '_' -> True
        _ -> C.isLetter x || C.isSpace x
