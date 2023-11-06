{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Mewlix.Interpreter.Module
( readModule
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Abstract.State
import Data.Text (Text)
import Data.HashSet (HashSet)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Mewlix.IO.DataFile (readDataFile)
import Mewlix.IO.Directory (isDirectoryPath, localizePath)
import Mewlix.IO.File (safeDoesFileExist, safeReadFile)
import Control.Applicative ((<|>))
import Lens.Micro.Platform ((^.))
import System.FilePath ((</>))
import Mewlix.Parser.Run (parseRoot)
import Mewlix.Interpreter.Exceptions

-- The output of module-searching.
-- The resulting filepath is important.
type SearchOutput = (FilePath, Module)

{- Get Module -}
----------------------------------------------------------------------------------------
readModule :: FilePath -> Bool -> Evaluator SearchOutput
readModule path isMain = if isStdModule path
    then getStdModule path
    else findModule path isMain

findModule :: FilePath -> Bool -> Evaluator SearchOutput
findModule path isMain = do
    let searchPaths :: [FilePath] -> Evaluator (Maybe (FilePath, Text))
        searchPaths []       = return Nothing
        searchPaths (x : xs) = safeDoesFileExist x >>| \case
            True  -> safeReadFile x >>| return . Just . (x,)
            False -> searchPaths xs

    localizedPath <- if isMain
        then return path
        else do
            mainPath <- asks (^.moduleInfoL.modulePathL)
            return $ localizePath mainPath path

    -- Include paths specified in the interpreter.
    include <- asks (^.evaluatorMetaL.includePathsL)
    flagset <- asks (^.evaluatorMetaL.flagSetL)

    let resolver = resolvePath flagset . (</> localizedPath)
    let paths    = localizedPath : map resolver include

    moduleCache <- cacheGet
    let cacheSearch :: [FilePath] -> Maybe SearchOutput
        cacheSearch = do
            let predicate :: FilePath -> Maybe SearchOutput -> Maybe SearchOutput
                predicate filepath acc = acc <|> do
                    foundModule <- HashMap.lookup filepath moduleCache
                    return (filepath, foundModule)
            foldr predicate Nothing

    -- Search module cache. If miss, search directories.
    case cacheSearch paths of
        (Just modu) -> return modu
        Nothing     -> searchPaths paths >>= \case
            (Just contents) -> uncurry makeModule contents
            Nothing         -> throwError $ importException path


{- Parsing -}
----------------------------------------------------------------------------------------
makeModule :: FilePath -> Text -> Evaluator SearchOutput
makeModule path contents = case parseRoot path contents of
    (Left exc)     -> throwError $ importSyntaxException path exc
    (Right output) -> do
        let newModule = Module output
        liftIO (print output)
        cacheAdd path newModule
        return (path, newModule)

{- Utils -}
----------------------------------------------------------------------------------------
resolvePath :: FlagSet -> FilePath -> FilePath
resolvePath flags path =
    if isDirectoryPath path && Set.member ImplicitMain flags
        then path </> "main.meows"
        else path

{- Module Cache -}
----------------------------------------------------------------------------------------
cacheGet :: Evaluator CacheMap
cacheGet = do
    state <- asks (^.evaluatorMetaL.cachedModulesL)
    readRef (getCache state)

cacheAdd :: FilePath -> Module -> Evaluator ()
cacheAdd path modu = do
    state <- asks (^.evaluatorMetaL.cachedModulesL)
    modifyRef (HashMap.insert path modu) (getCache state)

{- Standard Modules -}
----------------------------------------------------------------------------------------
stdModules :: HashSet FilePath
{-# INLINABLE stdModules #-}
stdModules = HashSet.fromList
    [ "std.meows"
    , "io.meows"
    , "time.meows"
    , "assert.meows"
    , "cat_tree.meows"
    , "hashmap.meows"
    , "hashset.meows"
    , "numbers.meows"
    , "exception.meows" ]

asStd :: FilePath -> FilePath
asStd = ("std/" ++)

isStdModule :: FilePath -> Bool
isStdModule = flip HashSet.member stdModules

getStdModule :: FilePath -> Evaluator SearchOutput
getStdModule path = do
    cache <- cacheGet
    let stdPath = asStd path
    case HashMap.lookup stdPath cache of
        (Just modu) -> return (stdPath, modu)
        Nothing     -> readDataFile stdPath >>| makeModule stdPath
