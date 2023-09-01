{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Interpreter.Module
(
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.State
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Meowscript.IO.DataFile (readDataFile)
import Meowscript.Utils.Types (FilePathT)
import Meowscript.IO.Directory (isDirectoryPath)
import Meowscript.IO.File (safeDoesFileExist, safeReadFile)
import Lens.Micro.Platform ((^.), view, set, over)
import System.FilePath ((</>))
import Meowscript.Parser.AST (Statement)
import Meowscript.Parser.Run (parseRoot)

{- Get Module -}
----------------------------------------------------------------------------------------

-- Try loading from cache:
loadModule :: FilePath -> Evaluator a Module
loadModule path = do
    flagSet <- askState (^.evaluatorMetaL.flagSetL)
    let resolve  = Text.pack . resolvePath flagSet
    let resolved = resolve path
    cacheSearch (resolve path) >>= \case
        Nothing     -> readModule resolved
        (Just modu) -> return modu

-- Try reading a file:
readModule :: FilePathT -> Evaluator a Module
readModule path = if isStdModule path
    then readStdFile path >>| makeModule unpacked
    else findModule unpacked
    where unpacked = Text.unpack path

-- Searching for a file:
findModule :: FilePath -> Evaluator a Module
findModule path = do
    -- Function to search each possible path safely.
    let search :: [FilePath] -> Evaluator a (Maybe (FilePath, Text.Text))
        search []       = return Nothing
        search (x : xs) = safeDoesFileExist x >>| \case
            True  -> safeReadFile x >>| return . Just . (x,)
            False -> search xs

    -- Include paths specified in the interpreter.
    include <- askState (^.evaluatorMetaL.includePathsL)
    flagSet <- askState (^.evaluatorMetaL.flagSetL)

    -- The 'path' parameter is already resolved!
    let resolver = resolvePath flagSet . (</> path)
    let paths    = path : map resolver include

    search paths >>= \case
        Nothing         -> undefined --throw "not found" error here
        (Just contents) -> uncurry makeModule contents


{- Parsing -}
----------------------------------------------------------------------------------------
makeModule :: FilePath -> Text.Text -> Evaluator a Module
makeModule path contents = case parseRoot path contents of
    (Left e)       -> undefined --throw parse error
    (Right output) -> do
        let newModule = Module output
        cacheAdd packed newModule
        return newModule
    where packed = Text.pack path

{- Utils -}
----------------------------------------------------------------------------------------
resolvePath :: FlagSet -> FilePath -> FilePath
resolvePath flags path =
    if isDirectoryPath path && Set.member ImplicitMain flags
        then path </> "main.meows"
        else path

{- Module Cache -}
----------------------------------------------------------------------------------------
cacheSearch :: FilePathT -> Evaluator a (Maybe Module)
cacheSearch path = do
    state <- askState (^.evaluatorMetaL.cachedModulesL)
    cache <- readRef (getCache state)
    return (HashMap.lookup path cache)

cacheAdd :: FilePathT -> Module -> Evaluator a ()
cacheAdd path modu = do
    state <- askState (^.evaluatorMetaL.cachedModulesL)
    modifyRef (HashMap.insert path modu) (getCache state)

{- Standard Modules -}
----------------------------------------------------------------------------------------
stdModules :: HashSet.HashSet Text.Text
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

isStdModule :: FilePathT -> Bool
isStdModule = flip HashSet.member stdModules

readStdFile :: FilePathT -> IO (Either Text.Text Text.Text)
{-# INLINABLE readStdFile #-}
readStdFile = readDataFile . Text.unpack . asStd
    where asStd = Text.append "std/"
