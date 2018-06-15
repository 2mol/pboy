-- |
{-# LANGUAGE OverloadedStrings #-}

module Web where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Text
import Network.Wreq
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Regex


regexpArXivID :: String -> Maybe (String,String,String,[String])
regexpArXivID = matchRegexAll ( mkRegex "^[0-9]{4}\\.[0-9]{4,5}?*$" )
--exStr = "1805.11547" ::
--  Just (_,match,_,_) ->
--    (_,match,_,_) <- regexpArXivID (unpack str)

makeArxivQuery :: Text -> Options
makeArxivQuery str = defaults & param "search_query" .~ [ "id:" `append` str]


{- getTagContent uses head internally, if we try to extract content from a non-existent tag, i.e. we couldn't find a title field, then we'll throw an exception. We return strictly ($!) in order to make sure the exception is thrown locally. -}
getArxivTitleUnsafe query = do
  r <-
    getWith query "https://export.arxiv.org/api/query"
  let title = fmap fromTagText $ getTagContent "title" (const True) $ getTagContent "entry" (const True) (parseTags (r ^. responseBody))
  return $! title


maybeGetArxivTitle name =
  do
  let m = regexpArXivID name
  case m of
    Nothing            -> return Nothing
    Just (_,match,_,_) -> --catch (fmap Just $
      catchJust (\(ErrorCall e) -> if True then Just () else Nothing)
      (fmap Just $ getArxivTitleUnsafe $ makeArxivQuery $ pack match)
      (\e -> return Nothing)
