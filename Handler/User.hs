module Handler.User where
import Import
import qualified Data.Text as T
import Data.Maybe

userForm :: Form User
userForm html = do
  Entity _ user <- lift requireAuth
  let ident      = userIdent user
      screenName = userScreenName user
      email      = userEmail user
      locale     = userLocale user
      profile    = userProfile user
      amazon     = userAmazon user
      langs      = [("Japanese" :: Text, "ja"), ("English", "en")]
  flip renderBootstrap html $ do
    User <$> pure ident
         <*> areq textField "screen_name" (Just screenName)
         <*> pure Nothing
         <*> aopt textField "email" (Just email)
         <*> areq (selectFieldList langs ) "locale" (Just locale)
         <*> (T.filter (/= '\r') . unTextarea <$> areq textareaField "profile" (Just $ Textarea profile))
         <*> aopt textField "Amazon Associate" (Just amazon)

banForm :: Form [Entity Banned]
banForm html = do
  Entity _ user <- lift requireAuth
  bans <- lift $ runDB $ filter (isJust . bannedIp . entityVal) <$> selectList [] []
  let bansSettings = FieldSettings { fsLabel = SomeMessage MsgBans
                                   , fsAttrs = [("class", "span8")]
                                   , fsName  = Just "delete-bans"
                                   , fsId    = Just "delete-bans"
                                   , fsTooltip = Nothing
                                   }
  flip renderBootstrap html $
    areq (multiSelectFieldList [(mkOptName b, e) | e@(Entity _ b) <- bans]) bansSettings Nothing
  where
    mkOptName b = T.pack $ fromJust $ bannedIp b

postBanSettingsR :: Handler RepHtml
postBanSettingsR = do
  Entity key _ <- requireAuth
  ((result, _), _) <- runFormPost banForm
  liftIO $ print result
  case result of
    FormSuccess bans -> do
         runDB $ mapM_ (delete . entityKey) bans
         redirect UserSettingsR
    _ -> permissionDenied "!!!!YOU ARE NOT ALLOWED TO CHANGE!!!!"

getUserSettingsR :: Handler RepHtml
getUserSettingsR = do
  Entity key usr <- requireAuth
  (widget, enctype) <- generateFormPost userForm
  (banWidget, banEnctype) <- generateFormPost banForm
  defaultLayout $ do
    setTitle "Settings"
    $(widgetFile "user-settings")

postUserSettingsR :: Handler RepHtml
postUserSettingsR = putUserSettingsR

putUserSettingsR :: Handler RepHtml
putUserSettingsR = do
  Entity key _ <- requireAuth
  ((result, _), _) <- runFormPost userForm
  liftIO $ print result
  case result of
    FormSuccess newUser -> do
         runDB $ replace key newUser
         redirect UserSettingsR
    _ -> permissionDenied "!!!!YOU ARE NOT ALLOWED TO EDIT OTHER'S PROFILE!!!!"
