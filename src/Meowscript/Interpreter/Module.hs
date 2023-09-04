{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Interpreter.Module
( readModule
) where

import Meowscript.Data.Ref
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.State
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Meowscript.IO.DataFile (readDataFile)
import Meowscript.IO.Directory (isDirectoryPath)
import Meowscript.IO.File (safeDoesFileExist, safeReadFile)
import Control.Applicative ((<|>))
import Lens.Micro.Platform ((^.))
import System.FilePath ((</>))
import Meowscript.Parser.Run (parseRoot)
import Meowscript.Interpreter.Exceptions

-- The output of module-searching.
-- "Why?" -> Because the resolved filepath is important!
type SearchOutput = (FilePath, Module)

{- Get Module -}
----------------------------------------------------------------------------------------
-- Try reading a file:
readModule :: FilePath -> Evaluator a SearchOutput
readModule path = if isStdModule path
    then getStdModule path
    else findModule path

-- Searching for a file:
findModule :: FilePath -> Evaluator a SearchOutput
findModule path = do
    -- Function to search each possible path safely.
    let search :: [FilePath] -> Evaluator a (Maybe (FilePath, Text.Text))
        search []       = return Nothing
        search (x : xs) = safeDoesFileExist x >>| \case
            True  -> safeReadFile x >>| return . Just . (x,)
            False -> search xs

    -- Include paths specified in the interpreter.
    include <- askState (^.evaluatorMetaL.includePathsL)
    flagset <- askState (^.evaluatorMetaL.flagSetL)

    -- The 'path' parameter is already resolved!
    let resolver = resolvePath flagset . (</> path)
    let paths    = path : map resolver include

    -- Loading module cache.
    cache   <- cacheGet
    let cacheSearch :: [FilePath] -> Maybe SearchOutput
        cacheSearch = foldr predicate Nothing
            where predicate filepath acc = acc <|> do
                    modu <- HashMap.lookup filepath cache
                    return (filepath, modu)

    -- Search module cache. If miss, search directories.
    case cacheSearch paths of
        (Just modu) -> return modu
        Nothing     -> search paths >>= \case
            (Just contents) -> uncurry makeModule contents
            Nothing         -> throwException $ importException path


{- Parsing -}
----------------------------------------------------------------------------------------
makeModule :: FilePath -> Text.Text -> Evaluator a SearchOutput
makeModule path contents = case parseRoot path contents of
    (Left exc)     -> throwException $ importSyntaxException path exc
    (Right output) -> do
        let newModule = Module output
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
cacheGet :: Evaluator a CacheMap
cacheGet = do
    state <- askState (^.evaluatorMetaL.cachedModulesL)
    readRef (getCache state)

cacheAdd :: FilePath -> Module -> Evaluator a ()
cacheAdd path modu = do
    state <- askState (^.evaluatorMetaL.cachedModulesL)
    modifyRef (HashMap.insert path modu) (getCache state)

{- Standard Modules -}
----------------------------------------------------------------------------------------
stdModules :: HashSet.HashSet FilePath
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

getStdModule :: FilePath -> Evaluator a SearchOutput
getStdModule path = do
    cache <- cacheGet
    let stdPath = asStd path
    case HashMap.lookup stdPath cache of
        (Just modu) -> return (stdPath, modu)
        Nothing     -> readDataFile stdPath >>| makeModule stdPath
