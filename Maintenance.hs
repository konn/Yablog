{-# LANGUAGE PatternGuards, OverloadedStrings, QuasiQuotes #-}
module Main where
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import qualified Data.ByteString.Char8 as BS
import Text.Hamlet
import Network.HTTP.Types
import Text.Blaze.Renderer.Utf8

main :: IO ()
main = run 3000 app

app :: Application
app req | ("static":ps) <- pathInfo req = do
  staticApp defaultWebAppSettings req { pathInfo = ps
                                      , rawPathInfo = BS.intercalate "/" $ drop 1 $ BS.split '/' $ rawPathInfo req
                                      }
app req = do
  return $ ResponseBuilder status200 [] $ renderHtmlBuilder [shamlet|
     !!!
     <html>
       <head>
         <title>Yablog is under maintenance
         <link rel="stylesheet" href="/static/css/bootstrap.css">
         <script type="text/javascript" src="/static/js/bootstrap.js" charset="utf-8">
       <body>
         <div .container>
           <header .hero-unit>
             <h1>Yablog is under maintenance now.
             <p .lead>Sorry for inconvenience...
  |]
