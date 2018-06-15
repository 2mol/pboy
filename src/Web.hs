-- |
{-# LANGUAGE OverloadedStrings #-}

module Web where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Lens.Micro
import           Network.Wreq
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.Regex

regexpArXivID :: String -> Maybe (String,String,String,[String])
regexpArXivID = matchRegexAll ( mkRegex "^[0-9]{4}\\.[0-9]{4,5}" )

makeArxivQuery :: T.Text -> Options
makeArxivQuery str = defaults & param "search_query" .~ [ "id:" `T.append` str]

{- getTagContent uses head internally, if we try to extract content from a non-existent tag, i.e. we couldn't find a title field, then we'll throw an exception. We return strictly ($!) in order to make sure the exception is thrown locally. -}
getArxivTitleUnsafe query = do
  r <-
    getWith query "https://export.arxiv.org/api/query"
  let title = fmap fromTagText $ getTagContent "title" (const True) $ getTagContent "entry" (const True) (parseTags (r ^. responseBody))
  return $! title


maybeGetArXivTitle name =
  do
  let m = regexpArXivID name
  case m of
    Nothing            -> return Nothing
    Just (_,match,_,_) ->
      -- Here we catch the possible empty head exception
      catchJust (\(ErrorCall e) -> if True then Just () else Nothing)
      ((fmap Just) . (fmap . fmap $ toText) $ getArxivTitleUnsafe $ makeArxivQuery $ T.pack match)
      (\e -> return Nothing)

toText :: BL.ByteString -> T.Text
toText = T.decodeUtf8 . BL.toStrict
