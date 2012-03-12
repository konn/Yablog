module Handler.User where
import Import
import qualified Data.Text as T

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

getUserSettingsR :: Handler RepHtml
getUserSettingsR = do
  Entity key usr <- requireAuth
  ((_, widget), enctype) <- generateFormPost userForm
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