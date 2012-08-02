{-# LANGUAGE OverloadedStrings #-}
module Main where
import Database.Persist.Store
import Database.Persist.MongoDB
import System.Environment
import Yesod.Default.Config (withYamlEnvironment, DefaultEnv(..))
import Control.Applicative

import Data.Text.IO
import Data.Text hiding (head)
import Prelude hiding (getLine, lines, putStr)
import qualified Prelude
import qualified Settings
import System.IO (hFlush, stdout)
import Model

main :: IO (User)
main = do
  [env] <- getArgs
  dbconf <- applyEnv =<< withYamlEnvironment "config/mongoDB.yml"
                           (read env :: DefaultEnv) loadConfig
  p <- createPoolConfig (dbconf :: Settings.PersistConfig)
  offset <- readLn
  flip (runMongoDBConn master) p $ do
    selectList [] [ OffsetBy offset, LimitTo 5, Desc ArticleCreatedDate
                  , Desc ArticleCreatedTime
                  ]
