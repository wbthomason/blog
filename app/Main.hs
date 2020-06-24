{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where


import           Control.Lens
import           Control.Monad
import           Data.Aeson                    as A
import           Data.Aeson.Lens
import           Data.Map                      as M
import           Data.Set                      as S
import           Data.Text
import           Data.Text.Internal.Search     as TS
import           Data.Time                     as TM
import           Development.Shake
import           Text.Pandoc
import           Text.Pandoc.Filter
import           Text.Pandoc.Highlighting
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           Development.Shake.Forward
import           GHC.Generics                   ( Generic )
import           Slick
import           System.Directory
import           Slick.Pandoc

import qualified Data.HashMap.Lazy             as HML
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           Data.List                      ( sortBy )
import           Data.Function                  ( on )
import           Data.Text.Lens                 ( unpacked )

---Config-----------------------------------------------------------------------

extensions :: Extensions
extensions = pandocExtensions <> extensionsFromList
  [ Ext_auto_identifiers
  , Ext_gfm_auto_identifiers
  , Ext_ascii_identifiers
  , Ext_tex_math_single_backslash
  ]

markdownOptions :: ReaderOptions
markdownOptions = def { readerExtensions = extensions }

html5Options :: WriterOptions
html5Options = def { writerHighlightStyle   = Just kate
                   , writerExtensions       = extensions
                   , writerHTMLMathMethod   = MathJax ""
                   , writerCiteMethod       = Citeproc
                   , writerWrapText         = WrapPreserve
                   , writerTopLevelDivision = TopLevelSection
                   , writerSectionDivs      = True
                   }

siteMeta :: SiteMeta
siteMeta = SiteMeta { siteAuthor    = "Wil Thomason"
                    , baseUrl       = "https://wbthomason.github.io/blog"
                    , siteTitle     = "something bloglike"
                    , twitterHandle = Nothing
                    , githubUser    = Nothing
                    }

outputFolder :: FilePath
outputFolder = "public/"

--Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta =
    SiteMeta { siteAuthor    :: String
             , baseUrl       :: String
             , siteTitle     :: String
             , twitterHandle :: Maybe String
             , githubUser    :: Maybe String
             }
    deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo =
  IndexInfo
    { posts :: [Post]
    } deriving (Generic, Show, FromJSON, ToJSON)

data Tag = Tag
  { tag :: String
  , tagPosts :: [Post]
  , tagUrl :: String
  } deriving (Generic, Show)

instance ToJSON Tag where
  toJSON Tag {..} =
    object ["tag" A..= tag, "posts" A..= tagPosts, "url" A..= tagUrl]


-- | Data for a blog post
data Post =
    Post { title       :: String
         , author      :: String
         , content     :: String
         , url         :: String
         , isoDate     :: String
         , prettyDate  :: String
         , srcPath     :: Maybe String
         , teaser      :: Maybe String
         , date        :: String
         , tags        :: [String]
         }
    deriving (Generic, Eq, Ord, Show, Binary)

instance FromJSON Post where
  parseJSON v = do
    let title = v ^. key "title" . _String . unpacked
        author = v ^. key "author" . _String . unpacked
        date = v ^. key "date" . _String . unpacked
        isoDate = formatDate date
        prettyDate = v ^. key "prettyDate" . _String . unpacked
        content = v ^. key "content" . _String . unpacked
        url = v ^. key "url" . _String . unpacked
        tags = v ^.. key "tags" . values . _String . unpacked
        srcPath = v ^? key "srcPath" . _String . unpacked
        teaser = v ^? key "teaser" . _String . unpacked
     in return Post {..}

instance ToJSON Post where
  toJSON Post {..} = object
    [ "title" A..= title
    , "author" A..= author
    , "content" A..= content
    , "url" A..= url
    , "tags" A..= tags
    , "isoDate" A..= isoDate
    , "prettyDate" A..= prettyDate
    , "teaser" A..= teaser
    , "date" A..= date
    , "srcPath" A..= srcPath
    ]

data AtomData =
  AtomData { title        :: String
           , domain       :: String
           , author       :: String
           , posts        :: [Post]
           , currentTime  :: String
           , atomUrl      :: String } deriving (Generic, ToJSON, Eq, Ord, Show)

-- | Pandoc conversion with pandoc-citeproc and pandoc-sidenote filters
filters :: [Filter]
filters = [JSONFilter "pandoc-citeproc", JSONFilter "pandoc-sidenote"]

-- | Handle possible pandoc failure within the Action Monad
unPandocM :: PandocIO a -> Action a
unPandocM p = do
  result <- liftIO $ runIO p
  either (fail . show) return result

loadUsingFilters
  :: PandocReader t -> PandocWriter -> ReaderOptions -> t -> Action Value
loadUsingFilters reader writer ropts txt = do
  pdoc@(Pandoc meta _) <-
    unPandocM $ reader txt >>= applyFilters ropts filters []
  meta'       <- flattenMeta writer meta
  outText     <- unPandocM $ writer pdoc
  withContent <- case meta' of
    Object m -> return . Object $ HM.insert "content" (String outText) m
    _        -> fail "Failed to parse metadata"
  return withContent

markdownToHTMLWithOptsAndFilters
  :: ReaderOptions -> WriterOptions -> Text -> Action Value
markdownToHTMLWithOptsAndFilters ropts wopts txt = loadUsingFilters reader
                                                                    writer
                                                                    ropts
                                                                    txt
 where
  reader = readMarkdown ropts
  writer = writeHtml5String wopts

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- compileTemplate' "templates/index.html"
  let indexInfo = IndexInfo { posts = posts' }
      indexHTML =
        T.unpack $ substitute indexT (withSiteMeta $ toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["posts//*.md"]
  forP pPaths buildPost

teaserIndex :: Text -> Maybe Int
teaserIndex postContent = case TS.indices "<!-- more -->" postContent of
  (idx : _) -> Just idx
  _         -> Nothing

getPostTeaser :: Text -> Maybe Text
getPostTeaser postContent =
  replace "<h1>" "<h2>"
    .   replace "<h2>" "<h3>"
    .   replace "<h3>" "<h4>"
    .   (\idx -> T.take idx postContent)
    <$> teaserIndex postContent

addTeaserLink :: Value -> Value
addTeaserLink =
  key "content"
    .  _String
    %~ (replace "<!-- more -->" "<span id=\"continue-reading\"></span>")

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  postData    <-
    markdownToHTMLWithOptsAndFilters markdownOptions html5Options
    . T.pack
    $ postContent
  let postTeaser     = getPostTeaser $ postData ^. key "content" . _String
      withPostTeaser = _Object . at "teaser" .~ (String <$> postTeaser)
  let teaserPostData = case postTeaser of
        Just _  -> addTeaserLink postData
        Nothing -> postData
  let postUrl =
        T.pack ((dropExtension . dropDirectory1 $ srcPath) </> "index.html")
      withPostUrl = _Object . at "url" ?~ String postUrl
  let prettyDate =
        prettyFormatDate $ postData ^. key "date" . _String . unpacked
      withPrettyDate = _Object . at "prettyDate" ?~ String (T.pack prettyDate)
  let fullPostData =
        withPrettyDate
          . withPostTeaser
          . withSiteMeta
          . withPostUrl
          $ teaserPostData
  template <- compileTemplate' "templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute
    template
    fullPostData
  convert fullPostData

ensureDirs :: Action ()
ensureDirs = do
  liftIO $ createDirectoryIfMissing True (outputFolder </> "css")

generateCSS :: Action ()
generateCSS = do
  filepaths <- getDirectoryFiles "sass" ["*.scss"]
  void $ forP filepaths $ \p -> cmd_
    ("sass" :: String)
    [EchoStdout False]
    [EchoStderr False]
    ["sass" </> p]
    [outputFolder </> "css" </> (replaceExtension p ".css")]

buildTags :: [Tag] -> Action ()
buildTags tags = do
  void $ forP tags writeTag

writeTag :: Tag -> Action ()
writeTag t@Tag { tagUrl } = do
  tagTempl <- compileTemplate' "templates/tag.html"
  writeFile' (outputFolder <> tagUrl -<.> "html") . T.unpack $ substitute
    tagTempl
    (withSiteMeta $ toJSON t)

getTags :: [Post] -> Action [Tag]
getTags posts = do
  let tagToPostsSet  = M.unionsWith mappend (toMap <$> posts)
      tagToPostsList = fmap S.toList tagToPostsSet
      tagObjects     = foldMapWithKey
        (\tag ps ->
          [Tag { tag, tagPosts = sortByDate ps, tagUrl = "/tag/" <> tag }]
        )
        tagToPostsList
  return tagObjects
 where
  toMap :: Post -> Map String (Set Post)
  toMap p@Post { tags } = M.unionsWith mappend (embed p <$> tags)
  embed :: Post -> String -> Map String (Set Post)
  embed post tag = M.singleton tag (S.singleton post)

sortByDate :: [Post] -> [Post]
sortByDate = sortBy (flip compareDates)
  where compareDates = compare `on` isoDate

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
 where
  parsedTime =
    parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" humanDate :: UTCTime

prettyFormatDate :: String -> String
prettyFormatDate humanDate = formatTime defaultTimeLocale "%d %b %Y" utcDate
 where
  utcDate =
    parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

buildFeed :: [Post] -> Action ()
buildFeed posts = do
  now <- liftIO TM.getCurrentTime
  let atomData = AtomData { title       = siteTitle siteMeta
                          , domain      = baseUrl siteMeta
                          , author      = siteAuthor siteMeta
                          , posts       = mkAtomPost <$> posts
                          , currentTime = toIsoDate now
                          , atomUrl     = "/atom.xml"
                          }
  atomTempl <- compileTemplate' "templates/atom.xml"
  writeFile' (outputFolder </> "atom.xml") . T.unpack $ substitute
    atomTempl
    (toJSON atomData)
 where
  mkAtomPost :: Post -> Post
  mkAtomPost p = p { date = formatDate $ date p }

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- sortByDate <$> buildPosts
  allTags  <- getTags allPosts
  buildTags allTags
  buildIndex allPosts
  buildFeed allPosts
  ensureDirs
  generateCSS

main :: IO ()
main = do
  let shOpts = forwardOptions $ shakeOptions { shakeVerbosity  = Normal
                                             , shakeLintInside = ["\\"]
                                             , shakeThreads    = 8
                                             }
  shakeArgsForward shOpts buildRules
