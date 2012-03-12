module Handler.Root where
import qualified Data.Text as T
import Control.Monad
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
  as <- runDB $ do
    as <- map entityVal <$> selectList [] [Asc ArticleCreatedDate, Asc ArticleCreatedTime, LimitTo 5 :: SelectOpt Article]
    zip as <$> mapM (get404 . articleAuthor) as
  articles <- mapM (\(art, auth) -> (,,) art auth <$> markupRender art) as
  defaultLayout $ do
    h2id <- lift newIdent
    setTitle "Yablog homepage"
    $(widgetFile "homepage")
