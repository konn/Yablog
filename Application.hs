{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplication
    , getApplicationDev
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Settings.Development
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import qualified Database.Persist.Store
import Network.HTTP.Conduit (newManager, def)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Blog
import Handler.User

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Yablog" resourcesYablog

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: AppConfig DefaultEnv Extra -> IO Application
getApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    logWare   = if development then logStdoutDev
                               else logStdout
makeFoundation :: AppConfig DefaultEnv Extra -> IO Yablog
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/mongoDB.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    return $ Yablog conf s p manager dbconf

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader getApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
