--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
  ( Compiler
  , Context
  , Item
  , Tags
  , applyAsTemplate
  , buildTags
  , compile
  , compressCssCompiler
  , constField
  , copyFileCompiler
  , create
  , dateField
  , defaultContext
  , defaultHakyllReaderOptions
  , defaultHakyllWriterOptions
  , fromCapture
  , fromList
  , getResourceBody
  , hakyll
  , idRoute
  , itemIdentifier
  , listField
  , loadAll
  , loadAndApplyTemplate
  , makeItem
  , match
  , pandocCompiler
  , pandocCompilerWithTransformM
  , recentFirst
  , relativizeUrls
  , route
  , setExtension
  , tagsField
  , tagsRules
  , templateBodyCompiler
  , unsafeCompiler
  )
import PandocFilterGraphviz (renderAll)
import Text.Pandoc.Walk (walkM)

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "fonts/**" $ do
      route idRoute
      compile copyFileCompiler
    match "graphviz-images/*.svg" $ do
      route idRoute
      compile copyFileCompiler
    match "mscgen-images/*.svg" $ do
      route idRoute
      compile copyFileCompiler
    match "static/**" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match (fromList ["about.markdown", "contact.markdown"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls
    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tag/*.html")
    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx =
              constField "title" title `mappend`
              listField "posts" (postCtxWithTags tags) (return posts) `mappend`
              defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/tag.html" ctx >>=
          loadAndApplyTemplate "templates/default.html" ctx >>=
          relativizeUrls
    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        customPostPandocCompiler >>=
        loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags) >>=
        loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags) >>=
        relativizeUrls
    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Archives" `mappend`
              defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
          loadAndApplyTemplate "templates/default.html" archiveCtx >>=
          relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Home" `mappend`
              defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

customPostPandocCompiler :: Compiler (Item String)
customPostPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (unsafeCompiler . walkM renderAll)
