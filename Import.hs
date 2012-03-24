module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Data.Monoid
    , module Control.Applicative
    , module Settings.StaticFiles
    , module Text.Pandoc
    , module Forms
    , Text
    , articleView
    , articleLink
    , makeSnippet
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
import qualified Data.Text as T
import Settings.StaticFiles
import Text.Pandoc hiding (Null)
import Yesod.Auth
import Data.Time
import Forms

articleView :: Article -> Widget
articleView article = do
  render <- lift getUrlRender
  let route = Just $ render $ ArticleR (toEnum $ articleCreatedDate article) (articleIdent article)
  musr <- lift maybeAuthId
  (author, tags) <- lift $ runDB $ do
    Entity key _ <- getBy404 $ UniqueArticle (articleCreatedDate article) (articleIdent article)
    author <- get404 (articleAuthor article)
    tags <- map (tagName . entityVal) <$> selectList [TagArticle ==. key] []
    return (userScreenName author, tags)
  body <- lift $ markupRender article
  blogTitle <- lift getBlogTitle
  let title  = articleTitle article
      editable = maybe False (== articleAuthor article) musr
      date = toEnum $ articleCreatedDate article
      posted = show $ UTCTime date (toEnum $ articleCreatedTime article)
      ident = articleIdent article
  $(widgetFile "article-view")

articleLink :: Article -> Route Yablog
articleLink article = ArticleR (toEnum $ articleCreatedDate article)
                               (articleIdent article)

makeSnippet :: Int -> Text -> Text
makeSnippet len t | T.length t <= len = t
                  | otherwise         = T.take len t `T.append` "..."

#if __GLASGOW_HASKELL__ < 740
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
