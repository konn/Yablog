module Handler.Blog where
import Data.Time
import Import
import Control.Monad
import Yesod.Auth
import qualified Data.Text as T
import Control.Applicative
import Data.Maybe
import Data.List (sortBy)
import Data.Function

articleForm :: Form (Article, [Text])
articleForm = articleForm' Nothing Nothing

articleForm' :: Maybe Article -> Maybe [Text] -> Form (Article, [Text])
articleForm' mart mtags htm = do
  Entity usrId usr <- lift requireAuth
  lift $ do
    accessible <- isAdmin usr
    unless accessible $ do
      permissionDenied "You are not in admins"
  now  <- liftIO getCurrentTime
  let day  = utctDay now
      time = timeToTimeOfDay $ utctDayTime now
  if maybe False ((/= usrId) . articleAuthor) mart
     then lift $ permissionDenied "You cannot edit that article."
     else flip renderBootstrap htm $
            let titleSettings = FieldSettings { fsLabel = MsgTitle
                                              , fsName = Just "title"
                                              , fsId = Just "title"
                                              , fsClass = ["span8"]
                                              , fsTooltip = Nothing
                                              }
                bodySettings  = FieldSettings { fsLabel = MsgArticle
                                              , fsId = Just "src"
                                              , fsName = Just "src"
                                              , fsClass = ["span8"]
                                              , fsTooltip = Nothing
                                              }
                tagsSettings  = FieldSettings { fsLabel = MsgTags
                                              , fsId = Just "tags"
                                              , fsName = Just "tags"
                                              , fsClass = ["span8"]
                                              , fsTooltip = Nothing
                                              }
                cDateSettings = FieldSettings { fsLabel = MsgCreatedDate
                                              , fsId = Just "created_date"
                                              , fsName = Just "created_date"
                                              , fsClass = []
                                              , fsTooltip = Nothing
                                              }
                cTimeSettings = FieldSettings { fsLabel   = MsgCreatedTime
                                              , fsName    = Just "created_time"
                                              , fsId      = Just "created_time"
                                              , fsClass   = []
                                              , fsTooltip = Nothing
                                              }
                art = Article <$> pure usrId
                              <*> areq textField titleSettings (articleTitle <$> mart)
                              <*> (T.unpack . T.filter (/='\r') . unTextarea <$>
                                    areq textareaField bodySettings
                                             (Textarea . T.pack . articleBody <$> mart))
                              <*> (fromEnum <$> areq dayField cDateSettings
                                                ((toEnum . articleCreatedDate <$> mart) <|> Just day))
                              <*> (fromEnum . timeOfDayToTime <$> areq timeField cTimeSettings
                                                ((timeToTimeOfDay . toEnum . articleCreatedTime <$> mart) <|> Just time))
                              <*> pure (articleModifiedAt =<< mart)
                tags = T.words . fromMaybe "" <$> aopt textField tagsSettings (Just . T.unwords <$> mtags)
            in (,) <$> art <*> tags

commentDeleteForm :: ArticleId -> Form [Comment]
commentDeleteForm art html = do
  let commentSettings = FieldSettings { fsLabel = MsgComments
                                      , fsClass = ["span8"]
                                      , fsName  = Just "delete-contents"
                                      , fsId    = Just "delete-contents"
                                      , fsTooltip = Nothing
                                      }
  cs <- lift $ runDB $ selectList [CommentArticle ==. art] []
  flip renderBootstrap html $
    areq (multiSelectFieldList [(mkOptName c, c) | Entity _ c <- cs]) commentSettings Nothing
  where
    mkOptName c = T.concat [ commentBody c, " - ", commentAuthor c, " / "
                           , T.pack$ show $ commentCreatedAt c
                           ]


commentForm' :: Maybe Comment -> ArticleId -> Form Comment
commentForm' mcom art html = do
  musr <- lift  maybeAuth
  time <- liftIO getCurrentTime
  let commentField = FieldSettings { fsLabel = MsgComment
                                   , fsClass = ["span8"]
                                   , fsName  = Just "comment-contents"
                                   , fsId    = Just "comment-contents"
                                   , fsTooltip = Nothing
                                   }
      nameField    = FieldSettings { fsLabel = MsgName
                                   , fsClass = []
                                   , fsId    = Just "comment-author"
                                   , fsName  = Just "comment-author"
                                   , fsTooltip = Nothing
                                   }
  flip renderBootstrap html $
    Comment <$> areq textField nameField (userScreenName . entityVal <$> musr)
            <*> (unTextarea <$> areq textareaField commentField (Textarea . commentBody <$> mcom))
            <*> pure (commentPassword =<< mcom)
            <*> pure time
            <*> pure art

commentForm :: Maybe Comment -> Article -> Form Comment
commentForm mcom art html = do
  Entity key _ <- lift $ runDB $ getBy404 $
                    UniqueArticle (articleCreatedDate art) (articleTitle art)
  commentForm' mcom key html

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
  (article, comments) <- runDB $ do
    Entity key article <- getBy404 (UniqueArticle (fromEnum date) title)
    cs <- map entityVal <$> selectList [CommentArticle ==. key] []
    return (article, cs)
  ((_, cWidget), cEnctype) <- generateFormPost $ commentForm Nothing article
  let mCommentForm = Just (cWidget, cEnctype)
  blogTitle <- getBlogTitle
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
           redirect $ ArticleR (toEnum $ articleCreatedDate article) (articleTitle article)
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
  

getRssR :: Day -> Text -> Handler RepRss
getRssR = undefined

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
