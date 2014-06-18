--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Control.Monad       ((>=>))
import           Data.Monoid         (mappend)
import qualified Data.Map            as M
import           System.FilePath     ((</>), splitFileName, takeDirectory, takeFileName)

import           Hakyll

import           Debug.Trace

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith configuration $ do
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
                            let (dir, fn) = splitFileName (toFilePath i)
                                datePath = replaceAll "-" (const "/") date in
                            dir </> datePath </> fn)
                          Nothing -> idRoute)

stripExtension :: Routes
stripExtension = setExtension "" `composeRoutes`
                 customRoute (\i -> toFilePath i ++ "/index.html")


--------------------------------------------------------------------------------
-- Compilers
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml = return . fmap (withUrls shorten)
  where shorten url = if takeFileName url == "index.html"
                        then takeDirectory url
                        else url

sanitizeUrls :: Item String -> Compiler (Item String)
sanitizeUrls = relativizeUrls >=> removeIndexHtml

--------------------------------------------------------------------------------
-- Contexts
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext


--------------------------------------------------------------------------------
-- Configuration
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
  { feedTitle       = "The Personal Blog of Jirka Maršík"
  , feedDescription = "Blogging about research in formal semantics."
  , feedAuthorName  = "Jiří Maršík"
  , feedAuthorEmail = "jiri.marsik@loria.fr"
  , feedRoot        = "http://www.loria.fr/~jmarsik"
  }

configuration :: Configuration
configuration = defaultConfiguration
  { deployCommand = "rsync -av _site/* jmarsik@loria.loria.fr:/local/web-homepages/jmarsik" }
