{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Control.Arrow

main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateCompiler

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/style.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "index.markdown" $ do
        route   $ constRoute "index.html"
        compile $ pageCompiler 
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

