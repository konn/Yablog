module Handler.Blog where
import Data.Time
import Import
import Control.Monad
import Yesod.Auth
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List (sortBy)
import Data.Function
import Text.Hamlet.XML
import Text.XML
import Blaze.ByteString.Builder
import Data.Maybe
import Network.HTTP.Conduit

postCreateR :: Handler RepHtml
postCreateR = do
  ((result, widget), enctype) <- runFormPost articleForm
  case result of
    FormSuccess (article, tags, tbs) -> do
      mapM_ (pingTrackback article) tbs
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
           redirect $ ArticleR (toEnum $ articleCreatedDate article) (articleIdent article)
         else do
           setMessageI $ MsgAlreadyExists $ articleTitle article
           defaultLayout $(widgetFile "post-article")
    _ -> do
      setMessageI MsgInvalidInput
      defaultLayout $(widgetFile "post-article")

getCreateR :: Handler RepHtml
getCreateR = do
  (widget, enctype) <- generateFormPost articleForm
  defaultLayout $ do
    $(widgetFile "post-article")

getArticleR :: YablogDay -> Text -> Handler RepHtml
getArticleR (YablogDay date) ident = do
  musr <- maybeAuthId
  (article, comments, trackbacks, mprev, mnext) <- runDB $ do
    Entity key article <- getBy404 (UniqueArticle (fromEnum date) ident)
    cs <- map entityVal <$> selectList [CommentArticle ==. key] []
    ts <- map entityVal <$> selectList [TrackbackArticle ==. key] []
    mnext <- selectFirst [ ArticleCreatedDate >=. articleCreatedDate article
                         , FilterOr [ ArticleCreatedDate >. articleCreatedDate article
                                    , ArticleCreatedTime >. articleCreatedTime article
                                    ]
                         ]
                         [ Asc ArticleCreatedDate, Asc ArticleCreatedTime]
    mprev <- selectFirst [ ArticleCreatedDate <=. articleCreatedDate article
                         , FilterOr [ ArticleCreatedDate <. articleCreatedDate article
                                    , ArticleCreatedTime <. articleCreatedTime article
                                    ]
                         ]
                         [ Desc ArticleCreatedDate, Desc ArticleCreatedTime ]
    return (article, cs, ts, entityVal <$> mprev, entityVal <$> mnext)
  (cWidget, cEnctype) <- generateFormPost $ commentForm Nothing article
  let mCommentForm = Just (cWidget, cEnctype)
  blogTitle <- getBlogTitle
  render <- getUrlRender
  defaultLayout $ do
    when (isJust mprev) $ do
      let prev = fromJust mprev
      addHamletHead
        [hamlet|
          <link rel="prev previous" href=@{articleLink prev} title=#{articleTitle prev}>
        |]
    when (isJust mnext) $ do
      let next = fromJust mnext
      addHamletHead
        [hamlet|
          <link rel="next" href=@{articleLink next} title=#{articleTitle next}>
        |]
    setTitle $ toHtml $ T.concat [articleTitle article, " - ", blogTitle]
    $(widgetFile "article")

pingTrackback :: Article -> String -> Handler ()
pingTrackback article tb = do
  renderUrl <- getUrlRender
  master <- getYesod
  blogName <- getBlogTitle
  let meta = [("title", T.encodeUtf8 $ articleTitle article)
             ,("excerpt", T.encodeUtf8 $ T.take 255 $ T.pack $ articleBody article)
             ,("url", T.encodeUtf8 $ renderUrl $ articleLink article)
             ,("blog_name", T.encodeUtf8 blogName)
             ]
      man = httpManager master
  lift $ flip httpLbs man . urlEncodedBody meta =<< parseUrl tb
  liftIO $ print meta
  return ()


putArticleR :: YablogDay -> Text -> Handler RepHtml
putArticleR (YablogDay day) ident = do
  ((result, widget), enctype) <- runFormPost articleForm
  usrId <- requireAuthId
  time  <- liftIO getCurrentTime
  case result of
    FormSuccess (article, tags, tbs) -> do
      mapM_ (pingTrackback article) tbs
      suc <- runDB $ do
        Entity key old <- getBy404 $ UniqueArticle (fromEnum day) ident
        if articleAuthor old == usrId
          then do
            replace key article { articleModifiedAt = Just time }
            mapM_ (delete . entityKey) =<< selectList [TagArticle ==. key] []
            mapM_ (insert . Tag key) tags
            return True
          else return False
      if suc
         then redirect $ ArticleR (YablogDay day) $ articleIdent article
         else permissionDenied "You are not allowed to edit this article."
    _ -> do
      setMessageI MsgInvalidInput
      let mCommentForm = Nothing :: Maybe (Widget, Text)
      defaultLayout $(widgetFile "edit-article")

getDeleteR :: YablogDay -> Text -> Handler RepHtml
getDeleteR = deleteArticleR 

getModifyR :: YablogDay -> Text -> Handler RepHtml
getModifyR (YablogDay day) ident = do
  (artId, art, tags) <- runDB $ do
    Entity key art <- getBy404 $ UniqueArticle (fromEnum day) ident
    tags <- map (tagName . entityVal) <$> selectList [TagArticle ==. key] []
    return (key, art, tags)
  (widget, enctype) <- generateFormPost $ articleForm' (Just art) (Just tags)
  (cWidget, cEnctype) <- generateFormPost $ commentDeleteForm artId
  let mCommentForm = Just (cWidget, cEnctype)
  defaultLayout $ do
    setTitleI $ MsgEdit $ articleTitle art
    $(widgetFile "edit-article")

postDeleteCommentR :: YablogDay -> Text -> Handler ()
postDeleteCommentR = deleteCommentR

postModifyR :: YablogDay -> Text -> Handler RepHtml
postModifyR = putArticleR

deleteArticleR :: YablogDay -> Text -> Handler RepHtml
deleteArticleR (YablogDay day) ident = do
  usrId  <- requireAuthId
  Entity key art <- runDB $ getBy404 $ UniqueArticle (fromEnum day) ident
  if articleAuthor art == usrId
    then do
      runDB $ do
        mapM_ (delete . entityKey) =<< selectList [TagArticle ==. key] []
        delete key
      redirect RootR
    else do
      permissionDenied "You are not allowed to delete that article."

postCommentR :: YablogDay -> Text -> Handler RepHtml
postCommentR (YablogDay date) ident = do
  Entity key article <- runDB $ getBy404 $ UniqueArticle (fromEnum date) ident
  ((result, _), _) <- runFormPost $ commentForm' Nothing key
  case result of
    FormSuccess comment -> do
      ans <- runDB $ insertBy comment
      case ans of
        Right _ -> do
           render <- getUrlRender
           let anchor = commentAnchor comment
           let url = T.concat [render $ ArticleR (toEnum $ articleCreatedDate article) (articleIdent article)
                              , "#", anchor
                              ]
           msgRender <- getMessageRender
           notice (articleAuthor article) (msgRender MsgNewComment) $
             T.unlines [ msgRender (MsgYouHaveNewCommentFor url)
                       , ""
                       , "\"" `T.append` commentBody comment `T.append` "\""
                       , "\nby " `T.append` commentAuthor comment
                       ]
           redirect url
        Left _ -> do
           setMessageI $ MsgAlreadyExists $ articleTitle article
           redirect $ ArticleR (toEnum $ articleCreatedDate article) (articleIdent article)
    _ -> do
      setMessageI MsgInvalidInput
      redirect $ ArticleR (toEnum $ articleCreatedDate article) (articleIdent article)

putCommentR :: YablogDay -> Text -> Handler ()
putCommentR = undefined

deleteCommentR :: YablogDay -> Text -> Handler ()
deleteCommentR (YablogDay day) ident = do
  Entity uid _ <- requireAuth
  Entity aid art <- runDB $ getBy404 $ UniqueArticle (fromEnum day) ident
  ((result, _), _) <- runFormPost $ commentDeleteForm aid
  when (uid /= articleAuthor art) $ do
    permissionDenied "You are not allowed to delete those comment(s)."
  case result of
    FormSuccess cs -> do
      when (any ((/= aid) . commentArticle) cs) $ permissionDenied "You can't delete that comment."
      runDB $ mapM_ (\c -> deleteBy $ UniqueComment aid (commentAuthor c) (commentCreatedAt c)) cs
      redirect $ ArticleR (YablogDay day) (articleIdent art)
    _ -> do
      setMessageI MsgInvalidInput
      redirect $ ModifyR (YablogDay day) (articleIdent art)

postPreviewR :: Handler RepHtml
postPreviewR = do
  author <- userScreenName . entityVal <$> requireAuth
  ((result, _), _) <- runFormPost articleForm
  case result of
    FormSuccess (article, tags, tbs) -> do
      let editable = False
          comments = []
          mCommentForm = Nothing :: Maybe (Widget, Text)
          title    = articleTitle article
          posted = show $ UTCTime (toEnum $ articleCreatedDate article) (toEnum $ articleCreatedTime article)
          date     = toEnum $ articleCreatedDate article :: Day
          route    = Nothing :: Maybe Text
          ident    = articleIdent article
          mnext    = Nothing
          mprev    = Nothing
      blogTitle <- getBlogTitle
      body <- markupRender Nothing article
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

postTrackbackR :: YablogDay -> Text -> Handler RepXml
postTrackbackR (YablogDay date) ident = do
  Entity aid _ <- runDB $ getBy404 $ UniqueArticle (fromEnum date) ident
  trackback <- runInputPost $
    Trackback aid <$> iopt textField "title"
                  <*> (liftM unTextarea <$> iopt textareaField "excerpt")
                  <*> ireq textField "url"
                  <*> iopt textField "blog_name"
  runDB $ do
    ans <- insertBy trackback
    case ans of
      Right _ -> return ()
      Left (Entity k _) -> replace k trackback
  liftIO $ print $ renderLBS def $ Document (Prologue [] Nothing []) (Element "response" [] [xml|<error>0|]) [] 
  return $ mkXmlResponse [xml|<error>0|]

getTrackbackR :: YablogDay -> Text -> Handler RepXml
getTrackbackR (YablogDay date) ident = do
  (art, ts) <- runDB $ do
    Entity aid art <- getBy404 $ UniqueArticle (fromEnum date) ident
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
            #{articleTitle art} - #{bTitle}
          <link>
            #{render $ ArticleR (YablogDay date) ident}
          <description>
            #{articleTitle art} - #{bTitle}
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
