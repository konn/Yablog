module Forms ( articleForm, articleForm', commentDeleteForm
             , commentForm, commentForm', trackbackForm
             , trackbackDeleteForm
             ) where
import Yesod.Form
import Prelude
import Model
import Control.Applicative
import Foundation
import Yesod hiding (Route(..))
import Data.Text (Text)
import Data.Time
import Control.Monad
import Data.Maybe
import Control.Arrow
import Markups
import Yesod.Default.Config
import qualified Network.Wai as W
import qualified Data.Text as T
import Data.Monoid

type URL = String
articleForm :: Form (Article, [Text], [URL], Maybe FileInfo)
articleForm = articleForm' Nothing Nothing

articleForm' :: Maybe Article -> Maybe [Text] -> Form (Article, [Text], [URL], Maybe FileInfo)
articleForm' mart mtags htm = do
  Entity usrId usr <- lift requireAuth
  lift $ do
    accessible <- isAdmin usr
    unless accessible $ do
      permissionDenied "You are not in admins"
  now  <- liftIO getCurrentTime
  askFiles >>= liftIO . print
  askParams >>= liftIO . print
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
                imageSettings = FieldSettings { fsLabel = SomeMessage MsgImage
                                              , fsTooltip = Nothing
                                              , fsName = Just "image"
                                              , fsId   = Just "image"
                                              , fsAttrs = []
                                              }
            in (,,,) <$> art <*> tags <*> tbs <*> fileAFormOpt imageSettings
       let appendFileWidget = [whamlet| <button .btn action="appendfile();">Append |]
       return (r, widget `mappend` appendFileWidget)

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
  ipaddr <- hostToString . W.remoteHost <$> lift waiRequest
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
  flip renderBootstrap html $
    Comment <$> areq textField nameField (userScreenName . entityVal <$> musr)
            <*> (unTextarea <$> areq textareaField commentField (Textarea . commentBody <$> mcom))
            <*> pure (commentPassword =<< mcom)
            <*> pure time
            <*> pure art
            <*> pure ipaddr

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
