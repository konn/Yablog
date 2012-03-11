module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Data.Monoid
    , module Control.Applicative
    , module Settings.StaticFiles
    , module Text.Pandoc
    , renderMarkdown
    , Text
#if __GLASGOW_HASKELL__ < 740
    , (<>)
#endif
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last)
import Yesod   hiding (Route(..))
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Settings.StaticFiles
import Text.Pandoc hiding (Null)
import Yesod.Default.Config

renderMarkdown :: String -> Html
renderMarkdown = writeHtml opts . readMarkdown defaultParserState
  where
    opts = defaultWriterOptions {writerHTMLMathMethod =  MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", writerHighlight = True, writerHtml5 = True }

#if __GLASGOW_HASKELL__ < 740
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
