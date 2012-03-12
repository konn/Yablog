module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Data.Monoid
    , module Control.Applicative
    , module Settings.StaticFiles
    , module Text.Pandoc
    , Text
    , articleView
#if __GLASGOW_HASKELL__ < 740
    , (<>)
#endif
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last)
import Yesod   hiding (Route(..))
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Settings.StaticFiles
import Text.Pandoc hiding (Null)
import Yesod.Auth
import Data.Time

articleView :: Article -> Widget
articleView article = do
  musr <- lift maybeAuthId
  (author, tags) <- lift $ runDB $ do
    Entity key _ <- getBy404 $ UniqueArticle (articleCreatedDate article) (articleTitle article)
    author <- get404 (articleAuthor article)
    tags <- map (tagName . entityVal) <$> selectList [TagArticle ==. key] []
    return (userScreenName author, tags)
  body <- lift $ markupRender article
  let title  = articleTitle article
      editable = maybe False (== articleAuthor article) musr
      date = toEnum $ articleCreatedDate article
      posted = show $ UTCTime date (toEnum $ articleCreatedTime article)
  $(widgetFile "article-view")


#if __GLASGOW_HASKELL__ < 740
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
