module Handler.Blog where
import Data.Time
import Import
import Control.Monad
import Yesod.Auth
import qualified Data.Text as T
import Data.List (sortBy)
import Data.Function
import Text.Hamlet.XML
import Text.XML
import Blaze.ByteString.Builder
import Data.Maybe
import System.Locale

postCreateR :: Handler RepHtml
postCreateR = do
  ((result, widget), enctype) <- runFormPost articleForm
  case result of
    FormSuccess (article, tags) -> do
      usr <- requireAuthId
      when (articleAuthor article /= usr) $ redirect RootR
      success <- runDB $ do
        ans <- insertBy article
        case ans of
          Right key -> do
            mapM_ (insertBy . Tag key) tags
            return True
          Left  _ -> return False
      if success
         then do
           redirect $ ArticleR (toEnum $ articleCreatedDate article) (articleTitle article)
         else do
           setMessageI $ MsgAlreadyExists $ articleTitle article
           defaultLayout $(widgetFile "post-article")
    _ -> do
      setMessageI MsgInvalidInput
      defaultLayout $(widgetFile "post-article")

getCreateR :: Handler RepHtml
getCreateR = do
  ((_, widget), enctype) <- generateFormPost articleForm
  defaultLayout $ do
    $(widgetFile "post-article")

getArticleR :: Day -> Text -> Handler RepHtml
getArticleR date title = do
  musr <- maybeAuthId
  (article, comments, trackbacks) <- runDB $ do
    Entity key article <- getBy404 (UniqueArticle (fromEnum date) title)
    cs <- map entityVal <$> selectList [CommentArticle ==. key] []
    ts <- map entityVal <$> selectList [TrackbackArticle ==. key] []
    return (article, cs, ts)
  ((_, cWidget), cEnctype) <- generateFormPost $ commentForm Nothing article
  let mCommentForm = Just (cWidget, cEnctype)
  blogTitle <- getBlogTitle
  render <- getUrlRender
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [title, " - ", blogTitle]
    $(widgetFile "article")

putArticleR :: Day -> Text -> Handler RepHtml
putArticleR day title = do
  ((result, widget), enctype) <- runFormPost articleForm
  usrId <- requireAuthId
  time  <- liftIO getCurrentTime
  case result of
    FormSuccess (article, tags) -> do
      suc <- runDB $ do
        Entity key old <- getBy404 $ UniqueArticle (fromEnum day) title
        if articleAuthor old == usrId
          then do
            replace key article { articleModifiedAt = Just time }
            mapM_ (delete . entityKey) =<< selectList [TagArticle ==. key] []
            mapM_ (insert . Tag key) tags
            return True
          else return False
      if suc
         then redirect $ ArticleR day $ articleTitle article
         else permissionDenied "You are not allowed to edit this article."
    _ -> do
      setMessageI MsgInvalidInput
      let mCommentForm = Nothing :: Maybe (Widget, Text)
      defaultLayout $(widgetFile "edit-article")

getDeleteR :: Day -> Text -> Handler RepHtml
getDeleteR = deleteArticleR 

getModifyR :: Day -> Text -> Handler RepHtml
getModifyR day title = do
  (artId, art, tags) <- runDB $ do
    Entity key art <- getBy404 $ UniqueArticle (fromEnum day) title
    tags <- map (tagName . entityVal) <$> selectList [TagArticle ==. key] []
    return (key, art, tags)
  ((_, widget), enctype) <- generateFormPost $ articleForm' (Just art) (Just tags)
  ((_, cWidget), cEnctype) <- generateFormPost $ commentDeleteForm artId
  let mCommentForm = Just (cWidget, cEnctype)
  defaultLayout $ do
    setTitleI $ MsgEdit title
    $(widgetFile "edit-article")

postDeleteCommentR :: Day -> Text -> Handler ()
postDeleteCommentR = deleteCommentR

postModifyR :: Day -> Text -> Handler RepHtml
postModifyR = putArticleR

deleteArticleR :: Day -> Text -> Handler RepHtml
deleteArticleR day title = do
  usrId  <- requireAuthId
  Entity key art <- runDB $ getBy404 $ UniqueArticle (fromEnum day) title
  if articleAuthor art == usrId
    then do
      runDB $ do
        mapM_ (delete . entityKey) =<< selectList [TagArticle ==. key] []
        delete key
      redirect RootR
    else do
      permissionDenied "You are not allowed to delete that article."

postCommentR :: Day -> Text -> Handler RepHtml
postCommentR date title = do
  Entity key article <- runDB $ getBy404 $ UniqueArticle (fromEnum date) title
  ((result, _), _) <- runFormPost $ commentForm' Nothing key
  case result of
    FormSuccess comment -> do
      ans <- runDB $ insertBy comment
      case ans of
        Right _ -> do
           render <- getUrlRender
           let anchor = commentAnchor comment
           let url = T.concat [render $ ArticleR (toEnum $ articleCreatedDate article) (articleTitle article)
                              , "#", anchor
                              ]
           notice (articleAuthor article) $
             T.unlines [ T.concat ["You have new comment on your article: "
                                  , url
                                  ]
                       , commentBody comment
                       , "by " `T.append` commentAuthor comment
                       ]
           redirect url
        Left _ -> do
           setMessageI $ MsgAlreadyExists $ articleTitle article
           redirect $ ArticleR (toEnum $ articleCreatedDate article) (articleTitle article)
    _ -> do
      setMessageI MsgInvalidInput
      redirect $ ArticleR (toEnum $ articleCreatedDate article) (articleTitle article)

putCommentR :: Day -> Text -> Handler ()
putCommentR = undefined

deleteCommentR :: Day -> Text -> Handler ()
deleteCommentR day title = do
  Entity uid _ <- requireAuth
  Entity aid art <- runDB $ getBy404 $ UniqueArticle (fromEnum day) title
  ((result, _), _) <- runFormPost $ commentDeleteForm aid
  when (uid /= articleAuthor art) $ do
    permissionDenied "You are not allowed to delete those comment(s)."
  case result of
    FormSuccess cs -> do
      when (any ((/= aid) . commentArticle) cs) $ permissionDenied "You can't delete that comment."
      runDB $ mapM_ (\c -> deleteBy $ UniqueComment aid (commentAuthor c) (commentCreatedAt c)) cs
      redirect $ ArticleR day title
    _ -> do
      setMessageI MsgInvalidInput
      redirect $ ModifyR day title

postPreviewR :: Handler RepHtml
postPreviewR = do
  author <- userScreenName . entityVal <$> requireAuth
  ((result, _), _) <- runFormPost articleForm
  case result of
    FormSuccess (article, tags) -> do
      let editable = False
          comments = []
          mCommentForm = Nothing :: Maybe (Widget, Text)
          title    = articleTitle article
          posted = show $ UTCTime (toEnum $ articleCreatedDate article) (toEnum $ articleCreatedTime article)
          date     = toEnum $ articleCreatedDate article :: Day
          route    = Nothing :: Maybe Text
      blogTitle <- getBlogTitle
      body <- markupRender article
      defaultLayout $ do
        $(widgetFile "article-view")
    _ -> notFound

getTagR :: Text -> Handler RepHtml
getTagR tag = do
  articles <- runDB $ do
    mapM (get404 . tagArticle . entityVal) =<< selectList [TagName ==. tag] []
  when (null articles) notFound
  defaultLayout $ do
    setTitleI $ MsgArticlesForTag tag
    $(widgetFile "tag")

postTrackbackR :: Day -> Text -> Handler RepXml
postTrackbackR date title = do
  Entity aid _ <- runDB $ getBy404 $ UniqueArticle (fromEnum date) title
  trackback <- runInputPost $
    Trackback aid <$> iopt textField "title"
                  <*> (liftM unTextarea <$> iopt textareaField "excerpt")
                  <*> ireq textField "url"
                  <*> iopt textField "blog_name"
  ans <- runDB $ insertBy trackback
  case ans of
    Right _ -> return $ mkXmlResponse [xml|<error>0|]
    Left  _ -> return $ mkXmlResponse
               [xml|
                 <error>1
                 <message>Already exists.
               |]

getTrackbackR :: Day -> Text -> Handler RepXml
getTrackbackR date title = do
  (art, ts) <- runDB $ do
    Entity aid art <- getBy404 $ UniqueArticle (fromEnum date) title
    ts <- map entityVal <$> selectList [TrackbackArticle ==. aid] []
    return (art, ts)
  render <- getUrlRender
  desc <- getBlogDescription
  bTitle <- getBlogTitle
  return $ mkXmlResponse [xml|
      <error>
        0
      <rss version="0.91">
        <channel>
          <title>
            #{title} - #{bTitle}
          <link>
            #{render $ ArticleR date title}
          <description>
            #{title} - #{bTitle}
          <language>
            ja
          $forall t <- ts
            <item>
              <title>
                #{fromMaybe (trackbackUrl t) $ trackbackTitle t}
              <link>
                #{trackbackUrl t}
              $maybe e <- trackbackExcerpt t
                <description>
                  #{e}
    |]

mkXmlResponse :: [Node] -> RepXml
mkXmlResponse nodes = RepXml $
  ContentBuilder (fromLazyByteString $ renderLBS def $
                     Document (Prologue [] Nothing []) body [])
                 Nothing
  where
    body = Element "response" [] nodes
