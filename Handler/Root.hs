module Handler.Root where
import qualified Data.Text as T
import Import
import Yesod.Feed
import Control.Arrow
import Data.Time
import Data.Maybe
import Data.List (last)
import Control.Monad

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  offset <- fromMaybe 0 <$> runInputGet (iopt intField "of")
  liftIO . print . reqGetParams =<< getRequest
  (articles, hasMore) <- runDB $ do
    as <- selectList [] [OffsetBy offset, LimitTo 5, Desc ArticleCreatedDate, Desc ArticleCreatedTime]
    hasMore <-
      if null as
        then return False
        else do
          let anc = entityVal $ last as
          olds <- count [ ArticleCreatedDate <=. articleCreatedDate anc
                        , FilterOr [ ArticleCreatedDate <.  articleCreatedDate anc
                                   , ArticleCreatedTime <.   articleCreatedTime anc
                                   ]
                        ]
          return (olds > 0)
    cs <- mapM (\(Entity key _) -> count [CommentArticle ==. key]) as
    ts <- mapM (\(Entity key _) -> count [TrackbackArticle ==. key]) as
    return $ (zip3 (map entityVal as) cs ts, hasMore)
  title <- getBlogTitle
  defaultLayout $ do
    setTitle $ toHtml $ "Home - " `T.append` title
    $(widgetFile "homepage")

getFeedR :: Handler RepAtomRss
getFeedR = do
  bTitle <- getBlogTitle
  desc <- getBlogDescription
  arts <- runDB $ map entityVal <$> selectList [] [ LimitTo 5
                                              , Desc ArticleCreatedDate
                                              , Desc ArticleCreatedTime
                                              ]
  let ts = map (uncurry UTCTime . ((toEnum . articleCreatedDate) &&& (toEnum . articleCreatedTime))) arts
           ++ mapMaybe articleModifiedAt arts
  es <- mapM toFeedEntry arts
  time <- if null ts then liftIO getCurrentTime else return $ maximum ts
  newsFeed $ Feed { feedTitle = bTitle
                  , feedLinkSelf = FeedR
                  , feedLinkHome = RootR
                  , feedDescription = toHtml desc
                  , feedLanguage = "ja"
                  , feedUpdated = time
                  , feedEntries = es
                  }

toFeedEntry :: Article -> Handler (FeedEntry (Route Yablog))
toFeedEntry art = do
  body <- markupRender art
  let time = UTCTime (toEnum $ articleCreatedDate art) (toEnum $ articleCreatedTime art)
  return FeedEntry { feedEntryLink    = ArticleR (toEnum $ articleCreatedDate art) (articleTitle art)
                   , feedEntryUpdated = fromMaybe time $ articleModifiedAt art
                   , feedEntryTitle   = articleTitle art
                   , feedEntryContent = body
                   }
