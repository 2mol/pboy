{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module PDFInfo
    (-- * Reading PDF info
      pdfInfo
    , PDFInfo(..)
    , PDFSize(..)
    , PDFEncryptionInfo(..)
    , PDFInfoError(..)
    -- * Internals
    , ParsePDFInfo
    , runParse
    , parse
    , parseSize
    , parseDate
    , parseEncrypted
    , readRight)
    where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception as E
import           Control.Monad.Except

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, parseTimeM)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           Prelude
import           System.Exit
import           System.Process.Text

-- | A type representing the output from the pdfinfo command.
data PDFInfo = PDFInfo {
    pdfInfoTitle        :: !(Maybe Text)    -- ^ Title
  , pdfInfoSubject      :: !(Maybe Text)    -- ^ Subject
  , pdfInfoAuthor       :: !(Maybe Text)    -- ^ Author: E.g. Chris Done
  , pdfInfoCreator      :: !(Maybe Text)    -- ^ Creator: E.g. Microsoft Office Word 2007
  , pdfInfoProducer     :: !(Maybe Text)    -- ^ Producer: E.g. Microsoft Office Word 2007
  , pdfInfoCreationDate :: !(Maybe UTCTime) -- ^ Creation Date
  , pdfInfoModDate      :: !(Maybe UTCTime) -- ^ Modification Date
  , pdfInfoTagged       :: !(Maybe Bool)    -- ^ Tagged?
  , pdfInfoPages        :: !(Maybe Integer) -- ^ Pages: E.g. 238
  , pdfInfoEncrypted    :: !(Maybe PDFEncryptionInfo) -- ^ Encryption information
  , pdfInfoPageSize     :: !(Maybe PDFSize) -- ^ Page: E.g. 595.32 x 841.92 pts (A4)
  , pdfInfoFileSize     :: !(Maybe Integer) -- ^ File: E.g. 4061737 bytes
  , pdfInfoOptimized    :: !(Maybe Bool)    -- ^ Optimized?
  , pdfInfoPDFVersion   :: !(Maybe Double)  -- ^ PDF: E.g. 1.5
  } deriving Show

-- | Possible things that can go wrong while reading the info.
data PDFInfoError
  = ParseError !String        -- ^ Couldn't parse a property value.
  | ProcessFailure !Text      -- ^ Process exited with this stderr.
  | ProcessError !IOException -- ^ Error to do with the pdfinfo process.
  | NoMessage                 -- ^ No message given.
  | SomeError String          -- ^ Some nonspecific error.
  deriving Show

-- | Size of the PDF in pts.
data PDFSize = PDFSize { pdfSizeW :: !Float, pdfSizeH :: !Float }
  deriving (Eq,Show)

-- | Encryption and restricted permissions
data PDFEncryptionInfo
  -- | Not encrypted
  = PDFNoEncryption
  -- | Encrypted with possible permission restrictions
  | PDFEncryption {
      pdfCanPrint            :: !(Maybe Bool) -- ^ Can the file be printed?
    , pdfCanCopy             :: !(Maybe Bool) -- ^ Can the file be copied?
    , pdfCanChange           :: !(Maybe Bool) -- ^ Can the file be changed?
    , pdfCanAddNotes         :: !(Maybe Bool) -- ^ Can notes be added?
    , pdfEncryptionAlgorithm :: !(Maybe Text) -- ^ Encryption algorithm: e.g. unknown, RC4, AES, AES-256
    }
  deriving (Eq,Show)

-- instance Error PDFInfoError where noMsg = NoMessage; strMsg = SomeError
newtype ParsePDFInfo a = ParsePDFInfo { runParse :: Either PDFInfoError a }
  deriving (Monad,Functor,MonadError PDFInfoError)
instance Applicative ParsePDFInfo where (<*>) = ap; pure = pure

-- | Run pdfinfo on the given file. Handles IO exceptions to do with
-- running the process.
pdfInfo :: MonadIO m => FilePath -> m (Either PDFInfoError PDFInfo)
pdfInfo path = liftIO $ loadInfo `E.catch` ioErrorHandler where
  loadInfo = do (code,out,err) <- readProcessWithExitCode "pdfinfo" ["-enc","UTF-8",path] ""
                case code of
                  ExitSuccess -> return (parse out)
                  ExitFailure{} -> return (Left (ProcessFailure err))
  ioErrorHandler = return . Left . ProcessError

-- | Parse PDFInfo's output.
parse :: Text -> Either PDFInfoError PDFInfo
parse out = runParse $
  PDFInfo <$> string props "Title"
          <*> string props "Subject"
          <*> string props "Author"
          <*> string props "Creator"
          <*> string props "Producer"
          <*> date "CreationDate"
          <*> date "ModDate"
          <*> bool props "Tagged"
          <*> integer "Pages"
          <*> encrypted "Encrypted"
          <*> size "Page size"
          <*> integer "File size"
          <*> bool props "Optimized"
          <*> floating "PDF version"
    where date = get parseDate
          size = get parseSize
          encrypted = get parseEncrypted
          floating = readIt
          integer = readIt
          readIt :: Read a => Text -> ParsePDFInfo (Maybe a)
          readIt = get readRight
          props = map split . T.lines $ out
          get = withProps props

type Props = [(Text,Text)]

-- | Look up a name in a finite map of "properties" and apply a (parsing)
-- function.
withProps :: Props -> (Text -> ParsePDFInfo a) -> Text -> ParsePDFInfo (Maybe a)
withProps properties f name =
  case lookup name properties of
    Just ok -> catchError (Just <$> (f $ T.strip ok))
                          (\_ -> return Nothing)
    Nothing -> return Nothing

split :: Text -> (Text, Text)
split = second (T.drop 1) . T.span (/=':')

string :: Props -> Text -> ParsePDFInfo (Maybe Text)
string props = withProps props return

bool :: Props -> Text -> ParsePDFInfo (Maybe Bool)
bool props = withProps props $ \yes -> return $ yes == "yes"

-- | Parse a page size. This is loosely defined.
parseSize :: Text -> ParsePDFInfo PDFSize
parseSize s =
  case T.words s of
    ((readRight -> Right x):"x":(readRight -> Right y):_) ->
        return $ PDFSize x y
    _ -> throwError $ ParseError $ "Unable to read size: " ++ show s

-- | Parse a date according to pdfinfo's format.
parseDate :: Text -> ParsePDFInfo UTCTime
parseDate s =
  case parseTimeM True defaultTimeLocale "%a %b %e %H:%M:%S %Y" (T.unpack s) of
    Just ok -> return ok
    Nothing -> throwError $ ParseError $ "Unable to parse date: " ++ show s

-- | Parse encryption information according to pdfinfo's format.
parseEncrypted :: Text -> ParsePDFInfo PDFEncryptionInfo
parseEncrypted s =
  case T.break isSpace s of
    ("yes",rest) ->
      PDFEncryption <$> bool props "print"
                    <*> bool props "copy"
                    <*> bool props "change"
                    <*> bool props "addNotes"
                    <*> string props "algorithm"
      where
        props = map split $ T.words $ T.filter (flip notElem ['(',')']) rest
    ("no",_) -> return PDFNoEncryption
    _ -> throwError $ ParseError $ "Unable to parse encryption: " ++ show s

-- | Read a value, maybe, allow misc trailing data.
readRight :: (MonadError PDFInfoError m,Read a) => Text -> m a
readRight s =
  case reads (T.unpack s) of
    [(v,_)] -> return v
    _ -> throwError $ ParseError $ "Couldn't read value: " ++ show s
