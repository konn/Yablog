module Handler.Blog where
import Data.Time
import Import
import Control.Monad
import Yesod.Auth
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortBy, isPrefixOf)
import Data.Function
import qualified Data.Map as M
import qualified Data.Conduit.Binary as BC
import Data.Conduit
import Text.Hamlet.XML
import Text.XML
import Text.XML.Cursor
import Blaze.ByteString.Builder
import Data.Maybe
import Network.HTTP.Conduit hiding (def)
import Network.HTTP.Types
import qualified Network.Wai as W
import System.Directory
import Control.Monad.Trans.Resource
import System.FilePath
import System.IO.Temp
import Network.URI

postCreateR :: Handler RepHtml
postCreateR = do
  ((result, widget), enctype) <- runFormPost articleForm
  case result of
    FormSuccess (article, tags, tbs) -> do
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
           errs <- catMaybes <$> mapM (pingTrackback article) tbs
           unless (null errs) $ setMessageI $ T.unlines errs
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
getArticleR = withArticle $ \(Entity key article) -> do
  let date = toEnum $ articleCreatedDate article
      ident = articleIdent article
  musr <- maybeAuthId
  (comments, trackbacks, mprev, mnext) <- runDB $ do
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
    return (cs, ts, entityVal <$> mprev, entityVal <$> mnext)
  (cWidget, cEnctype) <- generateFormPost $ commentForm Nothing article
  let mCommentForm = Just (cWidget, cEnctype)
  blogTitle <- getBlogTitle
  render <- getUrlRender
  defaultLayout $ do
    when (isJust mprev) $ do
      let prev = fromJust mprev
      toWidgetHead
        [hamlet|
          <link rel="prev previous" href=@{articleLink prev} title=#{articleTitle prev}>
        |]
    when (isJust mnext) $ do
      let next = fromJust mnext
      toWidgetHead
        [hamlet|
          <link rel="next" href=@{articleLink next} title=#{articleTitle next}>
        |]
    setTitle $ toHtml $ T.concat [articleTitle article, " - ", blogTitle]
    $(widgetFile "article")

pingTrackback :: Article -> String -> Handler (Maybe T.Text)
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
  rsp <- lift $ flip httpLbs man . urlEncodedBody meta =<< parseUrl tb
  if responseStatus rsp /= status200
    then return $ Just $ T.concat [ T.pack tb
                                  , ": HTTP Error: "
                                  , T.concat (LT.toChunks $ LT.decodeUtf8 $ responseBody rsp)
                                  ]
    else do
      case fromDocument <$> parseLBS def (responseBody rsp) of
        Right root -> do
          let code = T.concat $ root $// checkName (== "error") >=> descendant >=> content
              msgs = T.concat $ root $// checkName (== "message") >=> descendant >=> content
          if code == "0"
             then return Nothing
             else return $ Just $ T.concat [T.pack tb, ": ", msgs]
        Left  _ -> return $ Just $ T.concat [T.pack tb, ": malformed response"]

putArticleR :: YablogDay -> Text -> Handler RepHtml
putArticleR (YablogDay day) ident = do
  ((result, _), _) <- runFormPost articleForm
  usrId <- requireAuthId
  time  <- liftIO getCurrentTime
  case result of
    FormSuccess (article, tags, tbs) -> do
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
         then do
           errs <- catMaybes <$> mapM (pingTrackback article) tbs
           unless (null errs) $ setMessageI $ T.unlines errs
           redirect $ ArticleR (YablogDay day) $ articleIdent article
         else permissionDenied "You are not allowed to edit this article."
    _ -> do
      setMessageI MsgInvalidInput
      redirect $ ModifyR (YablogDay day) ident

getDeleteR :: YablogDay -> Text -> Handler RepHtml
getDeleteR = deleteArticleR 

getModifyR :: YablogDay -> Text -> Handler RepHtml
getModifyR = withArticleAuth $ \(Entity artId art) -> do
  tags <- runDB $ map (tagName . entityVal) <$> selectList [TagArticle ==. artId] []
  (widget, enctype) <- generateFormPost $ articleForm' (Just art) (Just tags)
  (cWidget, cEnctype) <- generateFormPost $ commentDeleteForm artId
  (tWidget, tEnctype) <- generateFormPost $ trackbackDeleteForm artId
  let mCommentTrackbackForm = Just (cWidget, cEnctype, tWidget, tEnctype)
      day   = toEnum $ articleCreatedDate art
      ident = articleIdent art
  mAttachments <- articleAttachments art
  defaultLayout $ do
    setTitleI $ MsgEdit $ articleTitle art
    addScript $ StaticR js_jquery_upload_1_0_2_min_js
    $(widgetFile "edit-article")

articleAttachments :: Article -> Handler (Maybe [FilePath])
articleAttachments art = liftIO $ do
  let dir = attachmentDir art
  exists <- doesDirectoryExist dir
  if exists
    then do
      children <- filter (not . ("." `isPrefixOf`)) <$> getDirectoryContents dir
      if null children
        then return Nothing
        else return $ Just $ map (("/" </> dir) </>) children
    else return Nothing

getDeleteAttachmentR :: YablogDay -> Text -> FilePath -> Handler ()
getDeleteAttachmentR d@(YablogDay day) ident fp = withArticleAuth act d ident
  where
    act (Entity artId art) = do
      usr <- requireAuthId
      tags <- runDB $ map (tagName . entityVal) <$> selectList [TagArticle ==. artId] []
      exc <- liftIO $ doesFileExist $ attachmentDir art </> fp
      unless exc $ do
        setMessageI $ MsgAttachmentNotFound fp
        redirect $ ModifyR (YablogDay day) ident
      liftIO $ removeFile $ attachmentDir art </> fp
      render <- getUrlRender
      redirect $ render (ModifyR (YablogDay day) ident) `T.append` "#attachments"

postDeleteCommentR :: YablogDay -> Text -> Handler ()
postDeleteCommentR = deleteCommentR

postModifyR :: YablogDay -> Text -> Handler RepHtml
postModifyR = putArticleR

deleteArticleR :: YablogDay -> Text -> Handler RepHtml
deleteArticleR = withArticleAuth $ \(Entity key _) -> do
  runDB $ do
    mapM_ (delete . entityKey) =<< selectList [TagArticle ==. key] []
    delete key
  redirect RootR

postCommentR :: YablogDay -> Text -> Handler RepHtml
postCommentR = withArticle $ \(Entity key article) -> do
  addr <- getIPAddrProxy
  isBanned <- isJust <$> runDB (selectFirst [BannedIp ==. Just addr] [])
  when isBanned $ permissionDenied "YOU ARE BANNED TO COMMENT"
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

deleteTrackbackR :: YablogDay -> Text -> Handler ()
deleteTrackbackR = withArticleAuth $ \(Entity aid art) -> do
  let day = toEnum $ articleCreatedDate art
      ident = articleIdent art
  Entity uid _ <- requireAuth
  Entity aid art <- runDB $ getBy404 $ UniqueArticle (fromEnum day) ident
  ((result, _), _) <- runFormPost $ trackbackDeleteForm aid
  when (uid /= articleAuthor art) $ do
    permissionDenied "You are not allowed to delete those comment(s)."
  case result of
    FormSuccess cs -> do
      when (any ((/= aid) . trackbackArticle) cs) $ permissionDenied "You can't delete that comment."
      runDB $ mapM_ (\c -> deleteBy $ UniqueTrackback aid (trackbackUrl c)) cs
      redirect $ ArticleR (YablogDay day) (articleIdent art)
    _ -> do
      setMessageI MsgInvalidInput
      redirect $ ModifyR (YablogDay day) (articleIdent art)

postDeleteTrackbackR :: YablogDay -> Text -> Handler ()
postDeleteTrackbackR = deleteTrackbackR

deleteCommentR :: YablogDay -> Text -> Handler ()
deleteCommentR = withArticleAuth $ \(Entity aid art) -> do
  let day = toEnum $ articleCreatedDate art
  ((result, _), _) <- runFormPost $ commentDeleteForm aid
  case result of
    FormSuccess (cs, ban) -> do
      when (any ((/= aid) . commentArticle) cs) $ permissionDenied "You can't delete that comment."
      runDB $ forM_ cs $ \c -> do
        deleteBy $ UniqueComment aid (commentAuthor c) (commentCreatedAt c)
        when ban $ void $ insert $ Banned (Just $ commentIpAddress c) Nothing
      redirect $ ArticleR (YablogDay day) (articleIdent art)
    _ -> do
      setMessageI MsgInvalidInput
      redirect $ ModifyR (YablogDay day) (articleIdent art)

postAttachR :: YablogDay -> Text -> Handler ()
postAttachR = withArticleAuth $ \(Entity _ article) -> do
  (_, files) <- runRequestBody
  case lookup "file" files of
    Just finfo -> do
      let dir = attachmentDir article
      liftIO $ do
        createDirectoryIfMissing True dir
      lift $ fileSource finfo $$ BC.sinkFile (dir </> T.unpack (fileName finfo))
    Nothing -> notFound

postPreviewR :: Handler RepHtml
postPreviewR = do
  author <- userScreenName . entityVal <$> requireAuth
  ((result, _), _) <- runFormPost articleForm
  case result of
    FormSuccess (article, tags, tbs) -> do
      let editable = False
          comments = []
          mCommentTrackbackForm = Nothing :: Maybe (Widget, Text, Widget, Text)
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
postTrackbackR = withArticle $ \(Entity aid article) -> do
  let date = toEnum $ articleCreatedDate article
      ident = articleIdent article
  trackback <- runInputPost $
    Trackback aid <$> iopt textField "title"
                  <*> (liftM unTextarea <$> iopt textareaField "excerpt")
                  <*> ireq textField "url"
                  <*> iopt textField "blog_name"
  man <- httpManager <$> getYesod
  curUrl <- getUrlRender <*> pure (ArticleR (YablogDay date) ident)
  alert <- lift $ do
    rsp <- flip httpLbs man =<< parseUrl (T.unpack $ trackbackUrl trackback)
    if responseStatus rsp == status200
       then
         if T.encodeUtf8 curUrl `BS.isInfixOf` BS.concat (LBS.toChunks (responseBody rsp))
         then return Nothing
         else return $ Just "Your page does not include link to my page."
       else return $ Just "HTTP Error"
  case alert of
    Nothing -> do
      runDB $ do
      ans <- insertBy trackback
      case ans of
        Right _ -> return ()
        Left (Entity k _) -> replace k trackback
      return $ mkXmlResponse [xml|<error>0|]
    Just err ->
        return $ mkXmlResponse [xml|<error>1
                                    <message>#{err}
                               |]

getTrackbackR :: YablogDay -> Text -> Handler RepXml
getTrackbackR (YablogDay date) ident = do
  (art, ts) <- runDB $ do
    Entity aid art <- getBy404 $ UniqueArticle (fromEnum date) ident
    ts <- map entityVal <$> selectList [TrackbackArticle ==. aid] []
    return (art, ts)
  render <- getUrlRender
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
    body = Element "response" M.empty nodes
