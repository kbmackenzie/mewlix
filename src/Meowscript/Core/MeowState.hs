{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.MeowState
( makeState
, makeState'
, meowSetPath
, cacheAdd
, cacheLookup
, meowCacheNew
, meowHasFlag
, implicitMain
, MeowFileOutput
, meowSearch
, meowResolve
, meowrRead
) where

import Meowscript.Core.AST
import Meowscript.Utils.Types
import Meowscript.Core.StdFiles
import Meowscript.Utils.IO
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.IORef (newIORef, readIORef, modifyIORef)
import Control.Monad.Reader (asks, liftIO)
import Control.Applicative (liftA2)
import Lens.Micro.Platform (set)
import System.FilePath (hasTrailingPathSeparator, (</>), isAbsolute)

{- Notes:
 - Flags are always lower-case. They're case-insenstiive. 
 - Built-in options keys are always lower-case; their values aren't.
 -
 - Special definitions are case-sensitive.
 - "What is that?" -> These:
 - meowr -specialDef=customValue
 -}

makeState :: FilePathT -> [Text.Text] -> IO ObjectMap -> Maybe MeowCache -> IO MeowState
makeState path args lib cache = return MeowState
    { _meowArgs     = args
    , _meowLib      = lib
    , _meowStd      = stdFiles
    , _meowCache    = cache
    , _meowPath     = path
    , _meowSocket   = Nothing
    , _meowInclude  = []
    , _meowFlags    = Set.empty
    , _meowDefines  = Map.empty }

makeState' :: FilePathT -> [Text.Text] -> IO ObjectMap -> IO MeowState
makeState' path args lib = meowCacheNew >>= makeState path args lib . Just

{- Getters and Setters -}
---------------------------------------------------------------------
meowSetPath :: FilePathT -> MeowState -> MeowState
meowSetPath = set meowPath

cacheAdd :: FilePathT -> Environment -> Evaluator ()
cacheAdd path env = asks (_meowCache . fst) >>= \case
    Nothing -> return ()
    (Just cache) -> liftIO $ modifyIORef cache (Map.insert path env)

cacheLookup :: FilePathT -> Evaluator (Maybe Environment)
cacheLookup path = asks (_meowCache . fst) >>= \case
    Nothing -> return Nothing
    (Just cache) -> Map.lookup path <$> (liftIO . readIORef) cache

meowCacheNew :: IO MeowCache
meowCacheNew = newIORef Map.empty

meowHasFlag :: [Text.Text] -> MeowState -> Bool
meowHasFlag keys state = let flags = _meowFlags state
    in any (`Set.member` flags) keys


{- Flag Types -}
---------------------------------------------------------------------
implicitMain :: [Text.Text]
implicitMain = ["i", "implicit", "implicitmain"]


{- Resolve path -}
-------------------------------------------------------------------------
type MeowFileOutput = (FilePath, Either Text.Text Text.Text)

meowResolve :: MeowState -> FilePath -> FilePath
{-# INLINABLE meowResolve #-}
meowResolve state path
    | isDir path && meowHasFlag implicitMain state = path </> "main.meows"
    | otherwise = path
    where isDir = liftA2 (||) hasTrailingPathSeparator (== ".")

meowSearch :: MeowState -> FilePath -> IO (Either Text.Text (MeowState, Text.Text))
{-# INLINABLE meowSearch #-}
meowSearch state path
    | isAbsolute path = fmap (state,) <$> readContents state path
    | otherwise = meowrFile state paths >>= \case
        (Left exception) -> (return . Left) exception
        (Right (foundPath, tryRead)) -> case tryRead of
            (Left exception) -> (return . Left) exception
            (Right contents) -> let state' = state { _meowPath = Text.pack foundPath }
                in return (Right (state', contents))
    where paths = (:) path $ map (</> path) (_meowInclude state)
          readContents = meowFileIO safeReadFile

meowFileIO :: (FilePath -> IO a) -> MeowState -> FilePath -> IO a
{-# INLINABLE meowFileIO #-}
meowFileIO f state = f . meowResolve state

meowrFile :: MeowState -> [FilePath] -> IO (Either Text.Text MeowFileOutput)
meowrFile _ [] = (return . Left) "File does not exist in path!"
meowrFile state (x:xs) = safeDoesFileExist x >>= \case
    (Left exception) -> (return . Left) exception
    (Right True) -> Right <$> meowrRead state x
    (Right False) -> meowrFile state xs

meowrRead :: MeowState -> FilePath -> IO (FilePath, Either Text.Text Text.Text)
{-# INLINABLE meowrRead #-}
meowrRead state path = readContents state path >>= \case
    (Left exc) -> return (path, Left exc)
    (Right contents) -> return (path, Right contents)
    where readContents = meowFileIO safeReadFile
