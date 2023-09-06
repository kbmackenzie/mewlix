{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Interpreter.Module
( readModule
) where

import Meowscript.Abstract.Meow
import Meowscript.Data.Ref
import Meowscript.Abstract.State
import Data.Text (Text)
import Data.HashSet (HashSet)
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Meowscript.IO.DataFile (readDataFile)
import Meowscript.IO.Directory (isDirectoryPath, localizePath)
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
readModule :: FilePath -> Bool -> Evaluator SearchOutput
readModule path isMain = if isStdModule path
    then getStdModule path
    else findModule path isMain

-- Searching for a file:
findModule :: FilePath -> Bool -> Evaluator SearchOutput
findModule path isMain = do
    -- Function to search each possible path safely.
    let search :: [FilePath] -> Evaluator (Maybe (FilePath, Text))
        search []       = return Nothing
        search (x : xs) = safeDoesFileExist x >>| \case
            True  -> safeReadFile x >>| return . Just . (x,)
            False -> search xs

    -- Localize module path if module being imported isn't
    -- the main module.
    truePath <- if isMain
        then return path
        else do
            mainPath <- asks (^.moduleInfoL.modulePathL)
            return $ localizePath mainPath path

    -- Include paths specified in the interpreter.
    include <- asks (^.evaluatorMetaL.includePathsL)
    flagset <- asks (^.evaluatorMetaL.flagSetL)

    -- The 'path' parameter is already resolved!
    let resolver = resolvePath flagset . (</> truePath)
    let paths    = truePath : map resolver include

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
