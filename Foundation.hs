module Foundation
    ( Yablog (..)
    , Route (..)
    , YablogMessage (..)
    , resourcesYablog
    , Handler
    , Widget
    , Form
    , YablogDate (..)
    , YablogDay  (..)
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    , getBlogTitle
    , getBlogDescription
    , markupRender
    , markupRender'
    , isAdmin
    , notice
    , commentAnchor
    , dayToString
    , hostToString
    , attachmentDir
    , getIPAddrProxy
    ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy            as L
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Data
import           Data.Default                    (def)
import           Data.List                       (isPrefixOf, nub, sort)
import           Data.Maybe
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Time
import           Database.Persist.MongoDB        hiding (master)
import qualified Database.Persist.Store
import           Markups
import           Model
import           Network.HTTP.Conduit            (Manager)
import           Network.HTTP.Types
import           Network.Mail.Mime
import           Network.Socket
import           Network.URI
import qualified Network.Wai                     as W
import           Prelude
import           Settings
import qualified Settings
import           Settings.Development
import           Settings.StaticFiles
import           System.FilePath                 hiding (joinPath)
import           System.IO.Unsafe
import           System.Locale
import           Text.Blaze.Html.Renderer.String
import           Text.Hamlet                     (hamletFile)
import           Text.Jasmine                    (minifym)
import           Text.Pandoc
import           Web.ClientSession               (getKey)
import           Yesod
import           Yesod.AtomFeed
import           Yesod.Auth
import           Yesod.Auth.BrowserId
import           Yesod.Auth.GoogleEmail
import           Yesod.Default.Config
import           Yesod.Default.Util              (addStaticContentExternal)
import           Yesod.ReCAPTCHA
import           Yesod.RssFeed
import           Yesod.Static

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Yablog = Yablog
    { settings      :: AppConfig DefaultEnv Extra
    , getStatic     :: Static -- ^ Settings for static file serving.
    , connPool      :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager   :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "Yablog" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype YablogRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Yablog = YablogRoute
-- * Creates the value resourcesYablog which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Yablog. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the YablogRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Yablog" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Yablog Yablog (FormResult x, Widget)

getBlogTitle :: GHandler sub Yablog T.Text
getBlogTitle = extraTitle . appExtra . settings <$> getYesod

getBlogDescription :: GHandler sub Yablog T.Text
getBlogDescription = extraDescription . appExtra . settings <$> getYesod

isAdmin :: User -> GHandler sub Yablog Bool
isAdmin usr = do
  as <- extraAdmins . appExtra . settings <$> getYesod
  return $ userIdent usr `elem` as

markupRender' :: Data a => Maybe String
              -> (a -> GHandler sub Yablog a)
              -> Article
              -> GHandler sub Yablog Html
markupRender' mid tran article = do
  extra <- appExtra . settings <$> getYesod
  usr <- runDB $ get404 $ articleAuthor article
  let markup = fromMaybe "markdown" $ articleMarkup article <|> extraMarkup extra
  let trans = bottomUpM tran . bottomUp (procAttach article)
              . maybe id (addAmazonAssociateLink . T.unpack) (userAmazon usr)
  renderMarkup mid markup trans $ articleBody article

markupRender :: Maybe String -> Article -> GHandler sub Yablog Html
markupRender mid = markupRender' mid (return :: Pandoc -> GHandler sub Yablog Pandoc)

dayToString :: Day -> String
dayToString = formatTime defaultTimeLocale "%Y%m%d"

attachmentDir :: Article -> FilePath
attachmentDir article =
    staticDir </> "files"
              </> dayToString (toEnum $ articleCreatedDate article :: Day)
              </> T.unpack (articleIdent article)

procAttach :: Article -> Inline -> Inline
procAttach article inl =
  case inl of
    Link  is targ -> Link  is $ rewriteUrl targ
    Image is targ -> Image is $ rewriteUrl targ
    _             -> inl
  where
    rewriteUrl t@(url, title)
        | isRelativeReference url && not ("/" `isPrefixOf` url)
            = (concat [ "/static/files/"
                      , dayToString (toEnum $ articleCreatedDate article)
                      , "/"
                      , T.unpack $ articleIdent article
                      , "/"
                      , url], title)
        | otherwise = t

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Yablog where
    approot = ApprootMaster $ appRoot . settings

    -- Place the session key file in the config folder
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
        musr <- maybeAuth
        let rss  = rssLink FeedR "RSS Link"
            atom = atomLink FeedR "Atom Link"
        accessible <- maybe (return False) (isAdmin . entityVal) musr
        master <- getYesod
        mmsg <- getMessage
        blogTitle <- getBlogTitle
        description <- getBlogDescription
        let mcse = case extraGoogleCSE (appExtra $ settings master) of
                     Just cse -> Just $(widgetFile "search")
                     Nothing  -> Nothing
        comments <- runDB $ do
          cs <- map entityVal <$> selectList [] [LimitTo 10, Desc CommentCreatedAt]
          as <- mapM (get404 . commentArticle) cs
          return $ zip cs as
        articles <- map entityVal <$> runDB (selectList [] [LimitTo 5, Desc ArticleCreatedDate, Desc ArticleCreatedTime])
        tags <- sort . nub . map (tagName . entityVal) <$> runDB (selectList [] [])
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        pc <- widgetToPageContent $ do
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
            addScriptRemote "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
            addScript $ StaticR js_bootstrap_js
            addStylesheet $ StaticR css_bootstrap_responsive_css
            addStylesheet $ StaticR css_bootstrap_css
            setTitle $ toHtml blogTitle
            recaptchaOptions def { theme = Just "clean" }
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

-- How to run database actions.
instance YesodPersist Yablog where
    type YesodPersistBackend Yablog = Action
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth Yablog where
    type AuthId Yablog = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing ->
                fmap Just $ insert $
                  User (credsIdent creds) (credsIdent creds) Nothing Nothing "ja" "" Nothing

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId, authGoogleEmail]

    authHttpManager = httpManager

-- Sends off your mail. Requires sendmail in production!
deliver :: Yablog -> L.ByteString -> IO ()
deliver _ = sendmail

instance YesodReCAPTCHA Yablog where
  recaptchaPublicKey  = getReCAPTCHA fst
  recaptchaPrivateKey = getReCAPTCHA snd

getReCAPTCHA :: ((T.Text, T.Text) -> b) -> GHandler sub Yablog b
getReCAPTCHA f = do
  mRe <- extraReCAPTCHA . appExtra . settings <$> getYesod
  case mRe of
    Nothing -> fail "ReCAPTCHA is not enabled."
    Just re -> return $ f re

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Yablog FormMessage where
    renderMessage _ _ = defaultFormMessage

newtype YablogDate = YablogDate { fromYablogDate :: UTCTime }
    deriving (Eq, Ord, FormatTime, ParseTime)

instance Show YablogDate where
  show = formatTime defaultTimeLocale "%Y-%m-%d"

instance Read YablogDate where
  readsPrec _ = readsTime defaultTimeLocale "%Y-%m-%d"

newtype YablogDay = YablogDay { unYablogDay :: Day }
    deriving (Show, Eq, Ord, ParseTime, Read, Enum)

instance PathPiece YablogDay where
  toPathPiece = T.pack . formatTime defaultTimeLocale "%Y%m%d" . unYablogDay
  fromPathPiece = Data.Time.parseTime defaultTimeLocale "%Y%m%d" . T.unpack

notice :: UserId -> T.Text -> T.Text -> Handler ()
notice usrId title msg = do
  usr <- runDB $ get404 usrId
  extra <- appExtra . settings <$> getYesod
  case (,) <$> extraMailAddress extra <*> userEmail usr of
    Just (addr, to) -> do
      let body = Part "text/plain; charset=utf-8" QuotedPrintableText Nothing [] $ LBS.fromChunks [T.encodeUtf8 msg]
          mail = Mail { mailFrom = Address (Just $ extraTitle extra) addr
                      , mailTo   = [Address (Just $ userScreenName usr) to]
                      , mailHeaders = [("Subject", title)]
                      , mailCc      = []
                      , mailBcc     = []
                      , mailParts   = [[body]]
                      }
      -- liftIO $ LBS.putStrLn =<< renderMail' mail
      liftIO $ renderSendMail mail
    Nothing -> return ()

commentAnchor :: Comment -> T.Text
commentAnchor c = T.concat [ "comment-"
                           , T.decodeUtf8 $ urlEncode True $ T.encodeUtf8 $ commentAuthor c
                           , "-"
                           , T.pack $ formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" $ commentCreatedAt c
                           ]
hostToString :: SockAddr -> String
hostToString (SockAddrUnix str) = str
hostToString (SockAddrInet _ host) = unsafePerformIO $ inet_ntoa host
hostToString addr@SockAddrInet6{} = unsafePerformIO $
                 fst `liftM` getNameInfo [NI_NUMERICHOST] True False addr >>=
                 maybe (fail "showsPrec: impossible internal error") return

getIPAddrProxy :: GHandler sub Yablog String
getIPAddrProxy = do
  waiReq <- waiRequest
  let addr = maybe (hostToString $ W.remoteHost waiReq) (T.unpack . T.decodeUtf8) $
               lookup "X-Forwarded-For" (W.requestHeaders waiReq)

  return addr
