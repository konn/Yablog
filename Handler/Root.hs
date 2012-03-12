module Handler.Root where
import qualified Data.Text as T
import Import

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  articles <- runDB $
    map entityVal <$> selectList [] [LimitTo 5, Desc ArticleCreatedDate, Desc ArticleCreatedTime]
  title <- getBlogTitle
  defaultLayout $ do
    h2id <- lift newIdent
    setTitle $ toHtml $ "Home - " `T.append` title
    $(widgetFile "homepage")
