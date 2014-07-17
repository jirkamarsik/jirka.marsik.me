--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Control.Monad       ((>=>), filterM)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         (mappend)
import qualified Data.Map            as M
import           System.FilePath     ((</>), normalise, splitFileName, takeDirectory,
                                      takeFileName)

import           Hakyll

import           Debug.Trace


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "pages/*" $ do
    route $ routeToRoot `composeRoutes`
            stripExtension
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= sanitizeUrls

  match "posts/*" $ do
    route $ routeToRoot         `composeRoutes`
            routeDatesToFolders `composeRoutes`
            stripExtension
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= sanitizeUrls

  match "research/*" $ do
    route stripExtension
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= sanitizeUrls

  match "research/*/*" $ do
    route idRoute
    compile copyFileCompiler

  match "research.html" $ do
    route stripExtension
    compile $ do
      research <- recentFirst =<< loadAll "research/*"
      publishedResearch <-
        filterM (\item -> do published <- getMetadataField (itemIdentifier item)
                                                           "published"
                             return $ published == Just "True") research
      let researchCtx = listField "publishedResearch" defaultContext
                                  (return publishedResearch) `mappend`
                        defaultContext
      getResourceBody
        >>= applyAsTemplate researchCtx
        >>= return . renderPandoc
        >>= loadAndApplyTemplate "templates/default.html" researchCtx
        >>= sanitizeUrls

  create ["archive/index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                       constField "title" "Archives"            `mappend`
                       defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= sanitizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx = listField "posts" postCtx (return posts) `mappend`
                     constField "title" "Home"                `mappend`
                     defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= sanitizeUrls

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <- take 10 <$> (recentFirst =<< loadAllSnapshots "posts/*" "content")
      renderRss myFeedConfiguration feedCtx posts

  match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
-- Routes
routeToRoot :: Routes
routeToRoot = customRoute (takeFileName . toFilePath)

routeDatesToFolders :: Routes
routeDatesToFolders = metadataRoute (\m ->
                        case M.lookup "date" m of
                          Just date -> customRoute (\i ->
                            let (dir, file) = splitFileName (toFilePath i)
                                datePath = replaceAll "-" (const "/") date in
                            dir </> datePath </> file)
                          Nothing -> idRoute)

stripExtension :: Routes
stripExtension = setExtension "" `composeRoutes`
                 customRoute (\i -> toFilePath i ++ "/index.html")


--------------------------------------------------------------------------------
-- Compilers
removeIndexHtmlFromUrls :: Item String -> Compiler (Item String)
removeIndexHtmlFromUrls = return . fmap (withUrls stripIndexHtml)

sanitizeUrls :: Item String -> Compiler (Item String)
sanitizeUrls = relativizeUrls >=> removeIndexHtmlFromUrls

getRelativeUrl :: Item a -> Compiler (Maybe String)
getRelativeUrl item = fmap (stripIndexHtml . normalise) <$>
                      getRoute (itemIdentifier item)


--------------------------------------------------------------------------------
-- Contexts
postCtx :: Context String
postCtx =
  field "titleJS" (\item ->
    maybe "Untitled post" escapeJsString <$>
          getMetadataField (itemIdentifier item) "title")   `mappend`
  field "identifierJS" (\item ->
    fromMaybe "POST-WITH-NO-ROUTE" <$> getRelativeUrl item) `mappend`
  field "canonicalUrlJS" (\item ->
    maybe webRoot (webRoot </>) <$> getRelativeUrl item)    `mappend`
  dateField "date" "%B %e, %Y"                              `mappend`
  defaultContext


--------------------------------------------------------------------------------
-- Utilities
stripIndexHtml :: FilePath -> FilePath
stripIndexHtml url = if takeFileName url == "index.html"
                       then takeDirectory url
                       else url

escapeJsString :: String -> String
escapeJsString = replaceAll "[\\'\"]" ("\\" ++)


--------------------------------------------------------------------------------
-- Configuration
webRoot :: String
webRoot = "http://jirka.marsik.me/"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
  { feedTitle       = "The Personal Blog of Jirka Maršík"
  , feedDescription = "Blogging about research in formal semantics."
  , feedAuthorName  = "Jiří Maršík"
  , feedAuthorEmail = "jiri.marsik89@gmail.com"
  , feedRoot        = webRoot
  }
