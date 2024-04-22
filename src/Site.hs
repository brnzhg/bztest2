--------------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

import           Control.Monad   (foldM)
import           Data.Binary     (Binary)
import           Data.Coerce     (coerce)
import           Data.List       (find)
import           Data.Maybe      (catMaybes, fromMaybe, isJust)
import           Data.Monoid     (mappend)

import           Hakyll

import           Data.Typeable   (Typeable)
import           System.FilePath (takeBaseName)



newtype DomainName = DomainName String
    deriving Eq
newtype DomainTgtKey = DomainTgtKey String
    deriving Eq

data DomainTarget = DomainTarget
    { targetKey :: DomainTgtKey -- this is what's in the Markup
    , target    :: Identifier
    --, backLinks :: [Identifier]
    }

--TODO replace with maps
newtype DomainLinks = DomainLinks
    { linkedTargetsById :: [(Identifier, [DomainTgtKey])]
    --, linkedIdsByTarget :: [(DomainTarget, [Identifier])]
    }

newtype DomainBacklinks = DomainBacklinks [(DomainTgtKey, [Identifier])]


--TODO maybe rename target args?
data DomainArgs = DomainArgs
    { domainName          :: DomainName
    , domainMakeTargetKey :: Identifier -> String -- note opposite of Tag pages, user decides location
    }


-- simple version, although could just register with filepath or register all
-- replace MonadMetadata with more general MonadDomainTargetFilter


buildDomainTargetsWith
    :: forall m. MonadMetadata m
    => DomainArgs
    -> (Identifier -> m Bool)
    -> Pattern
    -> m [DomainTarget]
buildDomainTargetsWith da targetFilter p = do
    ids <- getMatches p
    maybeTgts <- traverse getDefinedTgt ids
    return . catMaybes $ maybeTgts
    where
        getDefinedTgt :: Identifier -> m (Maybe DomainTarget)
        getDefinedTgt identifier = do
            filtered <- targetFilter identifier
            let dtk = domainMakeTargetKey da identifier
            return $ if filtered
                then Just (DomainTarget (DomainTgtKey dtk) identifier)
                else Nothing

getBackLinks :: DomainLinks -> DomainBacklinks
getBackLinks (DomainLinks ls) =
    foldr addLinkedTgts (DomainBacklinks []) ls
    where
        addLinkedTgts :: (Identifier, [DomainTgtKey]) -> DomainBacklinks -> DomainBacklinks
        addLinkedTgts (identifier, tgts) (DomainBacklinks lbt) =
            DomainBacklinks
                $ (\(dt, l) -> let l' = if dt `elem` tgts
                                            then identifier : l
                                            else l
                    in (dt, l'))
                <$> lbt

-- TODO in future need to parse through pandoc tree to find backlinks
-- this will be a different version probably and then force dependency?
--
-- currently praying getRoute bypasses the circularity
{-
buildDomainBackLinksWith
    :: forall m. MonadMetadata m
    => (Identifier -> m [DomainTgtKey])
    -> [DomainTarget]
    -> Pattern
    -> m DomainBacklinks
buildDomainBackLinksWith getLinkedTgts tgts p = do
    ids <- getMatches p
    foldM addLinkedTgts (DomainBacklinks []) ids
    where
        addLinkedTgts :: DomainBacklinks -> Identifier -> m DomainBacklinks
        addLinkedTgts (DomainBacklinks lbt) identifier = do
            linkedTgts <- getLinkedTgts identifier
            return . DomainBacklinks
                $ (\(dt, l) -> let l' = if targetKey dt `elem` linkedTgts
                                            then identifier : l
                                            else l
                    in (dt, l'))
                <$> lbt
-}



-- target-domains: ingredient
-- files with this metadata are registered as ingredients
simpleTargetFilter :: MonadMetadata m => DomainName -> Identifier -> m Bool
simpleTargetFilter dn identifier = do
    metadata <- getMetadata identifier
    return . fromMaybe False $ do
        ds <- lookupStringList "target-domains" metadata
        return $ elem dn (DomainName <$> ds)

-- ingredient: apple, orange, pear
-- a page's targets are under metadata field "domain"
{-
simpleGetLinkedTargets
    :: MonadMetadata m
    => DomainName -> Identifier -> m [DomainTgtKey]
simpleGetLinkedTargets (DomainName dn) identifier = do
    metadata <- getMetadata identifier
    return . maybe [] (fmap DomainTgtKey) $ lookupStringList dn metadata
-}
simpleGetDomainLinks :: forall m. MonadMetadata m =>
    DomainName -> Pattern -> m DomainLinks
simpleGetDomainLinks (DomainName dn) p = do
    ids <- getMatches p
    dls <- mapM getLinkedTargets ids
    return $ DomainLinks dls
    where
        getLinkedTargets :: Identifier -> m (Identifier, [DomainTgtKey])
        getLinkedTargets identifier = do
            metadata <- getMetadata identifier
            let linkedTgts = maybe [] (fmap DomainTgtKey) $ lookupStringList dn metadata
            return (identifier, linkedTgts)

--TODO coulod be helper taking function  (\matchedTarget -> Context String)
-- other fields - urlField, bodyField (just taking stuff off Item and returning repr)
targetField :: String -> DomainArgs -> [DomainTarget] -> Context a
targetField key da tgts = field key $ \item -> do
    let tk = DomainTgtKey . domainMakeTargetKey da . itemIdentifier $ item
    return . maybe "" (coerce . targetKey) $ find (\tgt -> tk == targetKey tgt) tgts

--TODO need outgoing links too? should that be made into a different version compiled, replace buildBacklinks with compileLinks
-- version will carry the link info, return Compiler Monad, or take in version and make Rules monad
-- maybe we do this second
-- this approach could be done for everything, outgoing links and incoming links store by identifier and version
-- when generate the actual file, make denepdency on other version of self and all incoming other version
--TODO field to point to all ingredients used



--------------------------------------------------------------------------------
-- maybe later: register ingredients list for recipe (can have more than one and merge), each ingredient marked with special quotes?
    -- for now first list item marks recipe key
-- recipes page and ingredients page
    -- both use definition lists, plus alphabet links. Nav page at top for letter of alphabet
    -- this is pure HTML i think, generated at end
-- home page has tag categories, just make this in markdown - Protein,

main :: IO ()
main = hakyll $ do
    let dn = DomainName "recipe"
    let da = DomainArgs dn (takeBaseName . toFilePath)

    tgts <- buildDomainTargetsWith da (simpleTargetFilter dn) "linked-posts/*"
    dl <- simpleGetDomainLinks dn "linked-posts/*"
    let bl = getBackLinks dl

    --backLinks <- buildDomainBackLinksWith (simpleGetLinkedTargets dn) tgts "linked-posts/*"

    -- for each identifier, look for it as a tgt -> tgt key -> backlink
    --     also look for it's links to tgt keys, and look up those linked targets
    --     noww we have linked context and backlink context
    match "linked-posts/*" $ do

        -- debugCompiler "debug" :: Compiler ()
        route $ setExtension "html"
        return ()

    --TODO make field mirroring tagsField
    --TODO use recipe template
    --TODO look at postlist for usage


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
