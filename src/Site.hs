--------------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Control.Monad (foldM)
import           Data.Maybe    (catMaybes, fromMaybe, isJust)
import           Data.Monoid   (mappend)


import           Hakyll


newtype DomainName = DomainName String
    deriving Eq
newtype DomainTgtKey = DomainTgtKey String
    deriving Eq

data DomainTarget = DomainTarget
    { targetKey :: DomainTgtKey -- this is what's in the Markup
    , target    :: Identifier
    --, backLinks :: [Identifier]
    }

newtype DomainBacklinks = DomainBacklinks [(DomainTarget, [Identifier])]

--TODO maybe rename target args?
data DomainArgs = DomainArgs
    { domainName          :: DomainName
    , domainMakeTargetKey :: Identifier -> String -- note opposite of Tag pages, user decides location
    }


-- simple version, although could just register with filepath or register all
-- replace MonadMetadata with more general MonadDomainTargetFilter
buildDomainTargetsWith
    :: MonadMetadata m
    => DomainArgs
    -> (Identifier -> m Bool)
    -> Pattern
    -> m [DomainTarget]
buildDomainTargetsWith da targetFilter p = do
    ids <- getMatches p
    maybeTgts <- traverse getDefinedTgt ids
    return . catMaybes $ maybeTgts
    where
        --getDefinedTgt :: Identifier -> m (Maybe DomainTarget)
        getDefinedTgt identifier = do
            filtered <- targetFilter identifier
            let dtk = domainMakeTargetKey da identifier
            return $ if filtered
                then Just (DomainTarget (DomainTgtKey dtk) identifier)
                else Nothing


-- TODO in future need to parse through pandoc tree to find backlinks
-- maybe use snaps for efficiency? not important for now
-- just makiing this simple for now
-- currently praying getRoute bypasses the circularity
buildDomainBackLinksWith
    :: forall m. MonadMetadata m
    => DomainName
    -> (Identifier -> m [DomainTgtKey])
    -> [DomainTarget]
    -> Pattern
    -> m DomainBacklinks
buildDomainBackLinksWith dn getLinkedTgts tgts p = do
    ids <- getMatches p
    foldM addLinkedTgts (DomainBacklinks []) ids
    where
        addLinkedTgts :: DomainBacklinks -> Identifier -> m DomainBacklinks
        addLinkedTgts (DomainBacklinks lbt) identifier = do
            linkedTgts <- getLinkedTgts identifier
            return . DomainBacklinks
                $ (\(dt, l)  -> (dt, if targetKey dt `elem` linkedTgts
                            then identifier : l
                            else l))
                <$> lbt



-- target-domains: ingredient
-- files with this metadata are registered as ingredients
simpleTargetFilter :: MonadMetadata m => Identifier -> DomainName -> m Bool
simpleTargetFilter identifier dn = do
    metadata <- getMetadata identifier
    return . fromMaybe False $ do
        ds <- lookupStringList "target-domains" metadata
        return $ elem dn (DomainName <$> ds)

-- ingredient: apple, orange, pear
-- a page's targets are under metadata field "domain"
simpleGetLinkedTargets
    :: MonadMetadata m
    => DomainName -> Identifier -> m [DomainTgtKey]
simpleGetLinkedTargets (DomainName dn) identifier = do
    metadata <- getMetadata identifier
    return . maybe [] (fmap DomainTgtKey) $ lookupStringList dn metadata



cool :: Compiler ()
cool = debugCompiler "yo"


main :: IO ()
main = do
    main2
    putStrLn "sup"

--------------------------------------------------------------------------------
-- maybe later: register ingredients list for recipe (can have more than one and merge), each ingredient marked with special quotes?
    -- for now first list item marks recipe key

-- recipes page and ingredients page
    -- both use definition lists, plus alphabet links. Nav page at top for letter of alphabet
    -- this is pure HTML i think, generated at end
-- home page has tag categories, just make this in markdown - Protein,

main2 :: IO ()
main2 = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
