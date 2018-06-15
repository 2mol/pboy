-- | 
{-# LANGUAGE OverloadedStrings #-}

module Web where

import Network.Wreq
import Control.Lens
import Data.Text
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

--exStr = "1805.11547" ::
makeArxivQuery :: Text -> Options
makeArxivQuery str = defaults & param "search_query" .~ [ "id:" `append` str]
getArxivTitle queryOpts = do
  r <- getWith queryOpts "https://export.arxiv.org/api/query"
  let title = fmap fromTagText $ getTagContent "title" (const True) $ getTagContent "entry" (const True) (parseTags (r ^. responseBody))
  return $ case title of
             (x:xs) -> x
             _      -> "No title found."
