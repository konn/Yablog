module Forms ( articleForm, articleForm', commentDeleteForm
             , commentForm, commentForm', trackbackForm
             , trackbackDeleteForm
             ) where
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.RWS          hiding (lift)
import           Control.Monad.Writer.Class
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time
import           Foundation
import           Markups
import           Model
import qualified Network.Wai                as W
import           Prelude
import           Yesod                      hiding (Route (..))
import           Yesod.Default.Config
import           Yesod.Form
import           Yesod.ReCAPTCHA

type URL = String
articleForm :: Form (Article, [Text], [URL])
articleForm = articleForm' Nothing Nothing

articleForm' :: Maybe Article -> Maybe [Text] -> Form (Article, [Text], [URL])
articleForm' mart mtags htm = do
  Entity usrId usr <- lift requireAuth
  lift $ do
    accessible <- isAdmin usr
    unless accessible $ do
      permissionDenied "You are not in admins"
  now  <- liftIO getCurrentTime
  fs <- askFiles
  let files = concat $ maybeToList (M.elems . M.filterWithKey (const . T.isPrefixOf "file") <$> fs)
  markup <- extraMarkup . appExtra . settings <$> lift getYesod
  ident <- maybe (lift newIdent) return $ articleIdent <$> mart
  let day  = utctDay now
      time = timeToTimeOfDay $ utctDayTime now
  if maybe False ((/= usrId) . articleAuthor) mart
     then lift $ permissionDenied "You cannot edit that article."
     else do
       (r, widget) <- flip renderBootstrap htm $
            let titleSettings = FieldSettings { fsLabel = SomeMessage MsgTitle
                                              , fsName = Just "title"
                                              , fsId = Just "title"
                                              , fsAttrs = [("class", "span8")]
                                              , fsTooltip = Nothing
                                              }
                bodySettings  = FieldSettings { fsLabel = SomeMessage MsgArticle
                                              , fsId = Just "src"
                                              , fsName = Just "src"
                                              , fsAttrs = [("class", "span8")]
                                              , fsTooltip = Nothing
                                              }
                tagsSettings  = FieldSettings { fsLabel = SomeMessage MsgTags
                                              , fsId = Just "tags"
                                              , fsName = Just "tags"
                                              , fsAttrs = [("class", "span8")]
                                              , fsTooltip = Nothing
                                              }
                identSettings = FieldSettings { fsLabel = SomeMessage MsgIdentifier
                                              , fsId = Just "ident"
                                              , fsName = Just "ident"
                                              , fsAttrs = [("class", "span8")]
                                              , fsTooltip = Nothing
                                              }
                cDateSettings = FieldSettings { fsLabel = SomeMessage MsgCreatedDate
                                              , fsId = Just "created_date"
                                              , fsName = Just "created_date"
                                              , fsAttrs = []
                                              , fsTooltip = Nothing
                                              }
                cTimeSettings = FieldSettings { fsLabel   = SomeMessage MsgCreatedTime
                                              , fsName    = Just "created_time"
                                              , fsId      = Just "created_time"
                                              , fsAttrs   = []
                                              , fsTooltip = Nothing
                                              }
                markupSettings = FieldSettings { fsLabel = "Markup"
                                               , fsName  = Just "markup"
                                               , fsId    = Just "markup"
                                               , fsAttrs = []
                                               , fsTooltip = Nothing
                                               }
                trackbackUrls = FieldSettings { fsLabel = "Trackback(s)"
                                              , fsName  = Just "trackbacks"
                                              , fsId    = Just "trackbacks"
                                              , fsAttrs = [("class", "span8")]
                                              , fsTooltip = Nothing
                                              }
                art = Article <$> pure usrId
                              <*> areq textField titleSettings (articleTitle <$> mart)
                              <*> areq textField identSettings (Just ident)
                              <*> aopt (selectFieldList $ map ((T.pack &&& id).fst) readers) markupSettings
                                       (Just $ (articleMarkup =<< mart) <|> markup)
                              <*> (T.unpack . T.filter (/='\r') . unTextarea <$>
                                    areq textareaField bodySettings
                                             (Textarea . T.pack . articleBody <$> mart))
                              <*> (fromEnum . fromMaybe day <$> aopt dayField cDateSettings
                                                (Just $ toEnum . articleCreatedDate <$> mart))
                              <*> (fromEnum . timeOfDayToTime . fromMaybe time <$> aopt timeField cTimeSettings
                                                (Just $ timeToTimeOfDay . toEnum . articleCreatedTime <$> mart))
                              <*> pure (articleModifiedAt =<< mart)
                tags = T.words . fromMaybe "" <$> aopt textField tagsSettings (Just . T.unwords <$> mtags)
                tbs  = maybe [] (lines . T.unpack . unTextarea) <$> aopt textareaField trackbackUrls Nothing
            in (,,) <$> art <*> tags <*> tbs
       return (r, widget)

commentDeleteForm :: ArticleId -> Form ([Comment], Bool)
commentDeleteForm art html = do
  let commentSettings = FieldSettings { fsLabel = SomeMessage MsgComments
                                      , fsAttrs = [("class", "span8")]
                                      , fsName  = Just "delete-contents"
                                      , fsId    = Just "delete-contents"
                                      , fsTooltip = Nothing
                                      }
      isSpamSettings = FieldSettings { fsLabel   = SomeMessage MsgIsSpam
                                     , fsAttrs   = []
                                     , fsName    = Just "report-as-spam"
                                     , fsId      = Just "report-as-spam"
                                     , fsTooltip = Nothing
                                     }
  cs <- lift $ runDB $ selectList [CommentArticle ==. art] []
  flip renderBootstrap html $
    (,) <$> areq (multiSelectFieldList [(mkOptName c, c) | Entity _ c <- cs]) commentSettings Nothing
        <*> areq checkBoxField isSpamSettings (Just False)
  where
    mkOptName c = T.concat [ commentBody c, " - ", commentAuthor c, " / "
                           , T.pack$ show $ commentCreatedAt c
                           ]

trackbackDeleteForm :: ArticleId -> Form [Trackback]
trackbackDeleteForm art html = do
  let tbsSettings = FieldSettings { fsLabel = SomeMessage MsgTrackbacks
                                  , fsAttrs = [("class", "span8")]
                                  , fsName  = Just "delete-tbs"
                                  , fsId    = Just "delete-tbs"
                                  , fsTooltip = Nothing
                                  }
  cs <- lift $ runDB $ selectList [TrackbackArticle ==. art] []
  flip renderBootstrap html $
    areq (multiSelectFieldList [(mkOptName c, c) | Entity _ c <- cs]) tbsSettings Nothing
  where
    mkOptName t = fromMaybe (fromMaybe (trackbackUrl t) $ trackbackBlogName t) $
                    trackbackTitle t

commentForm' :: Maybe Comment -> ArticleId -> Form Comment
commentForm' mcom art html = do
  mRecap <- lift $ extraReCAPTCHA . appExtra . settings <$> getYesod
  ipaddr <- lift getIPAddrProxy
  musr <- lift  maybeAuth
  time <- liftIO getCurrentTime
  let commentField = FieldSettings { fsLabel = SomeMessage MsgComment
                                   , fsAttrs = [("class", "span8")]
                                   , fsName  = Just "comment-contents"
                                   , fsId    = Just "comment-contents"
                                   , fsTooltip = Nothing
                                   }
      nameField    = FieldSettings { fsLabel = SomeMessage MsgName
                                   , fsAttrs = []
                                   , fsId    = Just "comment-author"
                                   , fsName  = Just "comment-author"
                                   , fsTooltip = Nothing
                                   }
      recap = if isJust mRecap then recaptchaAForm else pure ()
  flip renderBootstrap html $
    Comment <$> areq textField nameField (userScreenName . entityVal <$> musr)
            <*> (unTextarea <$> areq textareaField commentField (Textarea . commentBody <$> mcom))
            <*> pure (commentPassword =<< mcom)
            <*> pure time
            <*> pure art
            <*> pure ipaddr
            <*  recap

commentForm :: Maybe Comment -> Article -> Form Comment
commentForm mcom art html = do
  Entity key _ <- lift $ runDB $ getBy404 $
                    UniqueArticle (articleCreatedDate art) (articleIdent art)
  commentForm' mcom key html

trackbackForm :: ArticleId -> Form Trackback
trackbackForm aid = renderBootstrap $
  Trackback <$> pure aid
            <*> aopt textField titleSettings Nothing
            <*> (liftM unTextarea <$> aopt textareaField excerptSettings Nothing)
            <*> areq textField urlSettings Nothing
            <*> aopt textField blogSettings Nothing
  where
    titleSettings   = "title" { fsName = Just "title" }
    excerptSettings = "excerpt" { fsName = Just "excerpt" }
    urlSettings     = "url" { fsName = Just "url" }
    blogSettings    = "blog_name" { fsName = Just "blog_name" }
