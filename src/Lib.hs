module Lib where

import           Data.Either.Combinators (rightToMaybe)
import qualified Data.Maybe              as M
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
fileFile (Config {libraryDir}) origFilePath newFileName = do
    let newFilePath = libraryDir </> (T.unpack newFileName) <.> "pdf"
    D.createDirectoryIfMissing True libraryDir
    D.copyFile origFilePath newFilePath

fileNameSuggestions :: FilePath -> IO [Text]
fileNameSuggestions filePath = do
    plainTextContent <- P.readProcess "pdftotext" [filePath, "-"] ""

    pdfInfo <- PDFI.pdfInfo filePath

    let
        topContent =
            T.pack plainTextContent
                |> T.lines
                |> fmap stripCrap
                |> filter sanityCheck
                |> (take 4)

        infoTitle =
            rightToMaybe pdfInfo
                >>= PDFI.pdfInfoTitle
                |> fmap stripCrap

    pure $ case infoTitle of
                Just t  -> t : topContent
                Nothing -> topContent

-- TODO: create or use a filename sanitization function
-- sanitize :: Text -> Maybe Text
-- sanitize t = Just t

sanityCheck :: Text -> Bool
sanityCheck t = T.length t >= 3 && T.length t <= 64

-- TODO: strip _everything_: special characters, non-ascii, ...
stripCrap :: Text -> Text
stripCrap t = t
