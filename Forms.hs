module Forms ( articleForm, articleForm', commentDeleteForm
             , commentForm, commentForm', trackbackForm
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
import qualified Data.Text as T

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
  ident <- maybe (lift newIdent) return $ articleIdent <$> mart
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
                identSettings = FieldSettings { fsLabel = MsgIdentifier
                                              , fsId = Just "ident"
                                              , fsName = Just "ident"
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
                              <*> areq textField identSettings (Just ident)
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
