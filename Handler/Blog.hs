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
            let titleSettings = "Title"   { fsId = Just "title", fsClass = ["span8"] }
                bodySettings  = "Article" { fsId = Just "src", fsClass = ["span8"] }
                tagsSettings  = "Tags"    { fsId = Just "tags", fsClass = ["span8"] }
                art = Article <$> pure usrId
                              <*> areq textField titleSettings (articleTitle <$> mart)
                              <*> (T.unpack . T.filter (/='\r') . unTextarea <$>
                                    areq textareaField bodySettings
                                             (Textarea . T.pack . articleBody <$> mart))
                              <*> (fromEnum <$> areq dayField "created_date"
                                                ((toEnum . articleCreatedDate <$> mart) <|> Just day))
                              <*> (fromEnum . timeOfDayToTime <$> areq timeField "created_time"
                                                ((timeToTimeOfDay . toEnum . articleCreatedTime <$> mart) <|> Just time))
                              <*> pure (articleModifiedAt =<< mart)
                tags = T.words . fromMaybe "" <$> aopt textField tagsSettings (Just . T.unwords <$> mtags)
            in (,) <$> art <*> tags

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
getArticleR date text = do
  musr <- maybeAuthId
  (article, author, comments, tags) <- runDB $ do
    Entity key article <- getBy404 (UniqueArticle (fromEnum date) text)
    author <- get404 (articleAuthor article)
    comments <- map entityVal <$> selectList [CommentArticle ==. key] []
    tags <- map (tagName . entityVal) <$> selectList [TagArticle ==. key] []
    return (article, author, comments, tags)
  let title  = articleTitle article
      body   = renderMarkdown $ articleBody article
      editable = maybe False (== articleAuthor article) musr
  blogTitle <- getBlogTitle
  defaultLayout $ do
    setTitle $ toHtml $ T.concat [title, " - ", blogTitle]
    addScriptRemote "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
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
      defaultLayout $(widgetFile "edit-article")

getDeleteR :: Day -> Text -> Handler RepHtml
getDeleteR = deleteArticleR 

getModifyR :: Day -> Text -> Handler RepHtml
getModifyR day title = do
  (art, tags) <- runDB $ do
    Entity key art <- getBy404 $ UniqueArticle (fromEnum day) title
    tags <- map (tagName . entityVal) <$> selectList [TagArticle ==. key] []
    return (art, tags)
  ((_, widget), enctype) <- generateFormPost $ articleForm' (Just art) (Just tags)
  defaultLayout $ do
    setTitleI $ MsgEdit title
    $(widgetFile "edit-article")

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

postCommentR :: Day -> Text -> Handler ()
postCommentR = undefined

putCommentR :: Day -> Text -> Handler ()
putCommentR = undefined

deleteCommentR :: Day -> Text -> Handler ()
deleteCommentR = undefined

getRssR :: Day -> Text -> Handler RepRss
getRssR = undefined

postPreviewR :: Handler RepHtml
postPreviewR = do
  author <- entityVal <$> requireAuth
  ((result, _), _) <- runFormPost articleForm
  case result of
    FormSuccess (article, tags) -> do
      let editable = False
          comments = []
          body     = renderMarkdown $ articleBody article
          title    = articleTitle article
          date     = toEnum $ articleCreatedDate article
      defaultLayout $
        $(widgetFile "article")
    _ -> notFound

getTagR :: Text -> Handler RepHtml
getTagR tag = do
  articles <- runDB $ do
    mapM (get404 . tagArticle . entityVal) =<< selectList [TagName ==. tag] []
  when (null articles) notFound
  defaultLayout $ do
    setTitleI $ MsgArticlesForTag tag
    $(widgetFile "tag")
