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
    , makeBrief
    , withArticle
    , withArticleAuth
    , makeSnippet
#if __GLASGOW_HASKELL__ < 740
    , (<>)
#endif
    ) where
import           Control.Applicative  (pure, (<$>), (<*>))
import           Control.Monad
import           Data.Maybe
import           Data.Monoid          (Monoid (mappend, mempty, mconcat))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time
import           Forms
import           Foundation
import           Prelude              hiding (head, init, last, readFile, tail,
                                       writeFile)
import           Settings.Development
import           Settings.StaticFiles
import           Text.Pandoc          hiding (Null)
import           Yesod                hiding (Header (..), Route (..))
import           Yesod.Auth

articleView :: Maybe String -> Article -> Widget
articleView mid article = do
  render <- lift getUrlRender
  let route = Just $ render $ ArticleR (toEnum $ articleCreatedDate article) (articleIdent article)
  musr <- lift maybeAuthId
  (author, tags) <- lift $ runDB $ do
    Entity key _ <- getBy404 $ UniqueArticle (articleCreatedDate article) (articleIdent article)
    author <- get404 (articleAuthor article)
    tags <- map (tagName . entityVal) <$> selectList [TagArticle ==. key] []
    return (userScreenName author, tags)
  body <- lift $ markupRender mid article
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

makeBrief :: Article -> Article
makeBrief art@Article{articleBody=body} = art {articleBody = truncated}
  where
    Just reader = lookup (fromMaybe "markdown" $ articleMarkup art) readers
    Just writer = lookup (fromMaybe "markdown" $ articleMarkup art) writers
    opts = defaultWriterOptions
           { writerHTMLMathMethod =
               MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
           , writerHighlight = True
           , writerHtml5 = True
           , writerIdentifierPrefix = ""
           }
    pandoc@(Pandoc metas bs) = reader defaultParserState body
    truncated =
      case span (not . isPara) bs of
        (src, takeWhile (not . isHead) -> as) ->
          writer opts (Pandoc metas $ src ++ take 3 as)
    isPara (Para _) = True
    isPara _        = False
    isHead (Header _ _) = True
    isHead _          = False

withArticle :: (Entity Article -> Handler a) -> YablogDay -> Text -> Handler a
withArticle act (YablogDay day) ident = do
  act =<< runDB (getBy404 $ UniqueArticle (fromEnum day) ident)

withArticleAuth :: (Entity Article -> Handler a) -> YablogDay -> Text -> Handler a
withArticleAuth act = withArticle $ \ent@(Entity _ art) -> do
  Entity uid _ <- requireAuth
  when (uid /= articleAuthor art) $ do
    permissionDenied "You are not allowed to delete those comment(s)."
  act ent

#if __GLASGOW_HASKELL__ < 740
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
