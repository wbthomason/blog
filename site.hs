--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Monoid ((<>), mappend)
import Hakyll
import System.FilePath.Posix ((</>), takeBaseName, takeDirectory, takeFileName)

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $
  -- Images
   do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
  -- Javascript
    match "js/*" $ do
      route idRoute
      compile copyFileCompiler
  -- CSS/Style
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
  -- Content
    match "posts/*" $ do
      route $ cleanRoute
      compile $
        pandocCompiler >>= loadAndApplyTemplate "templates/default.html" postCtx >>=
        saveSnapshot "content" >>=
        loadAndApplyTemplate "templates/post.html" postCtx >>=
        relativizeUrls >>=
        cleanIndexUrls
  -- Templates
    match "templates/*" $ compile templateBodyCompiler
  -- index
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx
              -- Sections
             =
              listField "posts" previewCtx (return posts) <>
              constField "title" "Home" <>
              mainContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls

previewCtx :: Context String
previewCtx =
  dateField "date" "%B %e, %Y" `mappend` teaserField "teaser" "content" `mappend`
  mainContext

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` mainContext

-- Routes
-- Taken from https://www.rohanjain.in/hakyll-clean-urls/
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where
        p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern = "/index.html"
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
  | idx `isSuffixOf` url = take (length url - length idx) url
  | otherwise = url
  where
    idx = "index.html"

-- Site variables
twitter = "wilthomason"

github = "wbthomason"

site_title = "something bloglike"

base_url = ""

linkedin = "wbthomason"

email = "wbthomason@{my department, abbr.}.{my university}.edu"

author = "Wil Thomason"

-- Contexts
mainContext :: Context String
mainContext -- Variables
 =
  constField "twitter" twitter `mappend` constField "github" github `mappend`
  constField "base_url" base_url `mappend`
  constField "site_title" site_title `mappend`
  constField "linkedin" linkedin `mappend`
  constField "email" email `mappend`
  constField "author" author `mappend`
  defaultContext

-- Pagination
grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper = liftM (paginateEvery 2) . sortRecentFirst

makeId :: PageNumber -> Identifier
makeId n =
  fromFilePath $
  if (n == 1)
    then "index.html"
    else show n ++ "/index.html"
