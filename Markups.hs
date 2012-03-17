{-# LANGUAGE PatternGuards, OverloadedStrings #-}
module Markups (addAmazonAssociateLink, readers, renderMarkup) where
import Text.Pandoc.Generic
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Network.HTTP.Types
import Data.List
import qualified Data.CaseInsensitive as CI
import Blaze.ByteString.Builder
import Text.Pandoc
import Yesod
import Data.Maybe
import Prelude
import Data.Char

renderTwitterLink :: Pandoc -> Pandoc
renderTwitterLink = bottomUp go
  where
    go (Link is (url, title))
         | ("twitter":xs) <- T.split (==':') $ T.pack url
         = Link is (T.unpack $ T.append "http://twitter.com/#!/" $ T.intercalate ":" xs, title)
    go i = i

addAmazonAssociateLink :: String -> Pandoc -> Pandoc
addAmazonAssociateLink = bottomUp . procAmazon

renderMarkup :: String             -- ^ markup language
             -> (Pandoc -> Pandoc) -- ^ pandoc transformer
             -> String             -- ^ source
             -> Html               -- ^ Html
renderMarkup lang trans =
    writeHtml opts
                  . trans . renderTwitterLink
                  . fromMaybe readMarkdown (lookup (map toLower lang) readers) defaultParserState
  where
    opts = defaultWriterOptions
           { writerHTMLMathMethod =
               MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
           , writerHighlight = True
           , writerHtml5 = True
           }


procAmazon :: String -> Inline -> Inline
procAmazon tag (Link is (url, title))  = Link is (attachTo tag url, title)
procAmazon tag (Image is (url, title)) = Image is (attachTo tag url, title)
procAmazon _   il                      = il

amazons :: [T.Text]
amazons = "www.amazon.com":"amazon.com":concatMap (\cc -> [T.concat [www,"amazon.",co,cc] | www <- ["","www."], co <- ["co.", ""]]) ccTLDs

attachTo :: String -> String -> String
attachTo key url | (p@("http:":"":amazon:paths), qs) <- decodePath (BS.pack url)
                 , amazon `elem` amazons
                 , let cipath = map CI.mk paths
                 , ["o", "asin"] `isPrefixOf` cipath || "dp" `elem` cipath
                                  || ["gp", "product"] `isPrefixOf` cipath
                 = tail $ BS.unpack $ toByteString $ encodePath p (("tag", Just $ BS.pack key):qs)
attachTo _   url = url


ccTLDs :: [T.Text]
ccTLDs = ["ac", "ad", "ae", "af", "ag", "ai", "al", "am", "an", "ao", "aq", "ar", "as", "at", "au", "aw", "az", "ba", "bb", "bd", "be", "bf", "bg", "bh", "bi", "bj", "bm", "bn", "bo", "br", "bs", "bt", "bv", "bw", "by", "bz", "ca", "cc", "cd", "cf", "cg", "ch", "ci", "ck", "cl", "cm", "cn", "co", "cr", "cu", "cv", "cx", "cy", "cz", "de", "dj", "dk", "dm", "do", "dz", "ec", "ee", "eg", "eh", "er", "es", "et", "fi", "fj", "fk", "fm", "fo", "fr", "ga", "gd", "ge", "gf", "gg", "gh", "gi", "gl", "gm", "gn", "gp", "gq", "gr", "gs", "gt", "gu", "gw", "gy", "hk", "hm", "hn", "hr", "ht", "hu", "id", "ie", "il", "im", "in", "io", "iq", "ir", "is", "it", "je", "jm", "jo", "jp", "ke", "kg", "kh", "ki", "km", "kn", "kp", "kr", "kw", "ky", "kz", "la", "lb", "lc", "li", "lk", "lr", "ls", "lt", "lu", "lv", "ly", "ma", "mc", "md", "mg", "mh", "mk", "ml", "mm", "mn", "mo", "mp", "mq", "mr", "ms", "mt", "mu", "mv", "mw", "mx", "my", "mz", "na", "nc", "ne", "nf", "ng", "ni", "nl", "no", "np", "nr", "nu", "nz", "om", "pa", "pe", "pf", "pg", "ph", "pk", "pl", "pm", "pn", "pr", "ps", "pt", "pw", "py", "qa", "re", "ro", "ru", "rw", "sa", "sb", "sc", "sd", "se", "sg", "sh", "si", "sj", "sk", "sl", "sm", "sn", "so", "sr", "st", "sv", "sy", "sz", "tc", "td", "tf", "tg", "th", "tj", "tk", "tm", "tn", "to", "tp", "tr", "tt", "tv", "tw", "tz", "ua", "ug", "uk", "um", "us", "uy", "uz", "va", "vc", "ve", "vg", "vi", "vn", "vu", "wf", "ws", "ye", "yt", "yu", "za", "zm", "zw"]
