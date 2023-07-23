{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.MeowState
( meowState
, meowState'
, meowSetPath
, cacheAdd
, cacheLookup
, meowCacheNew
, meowHasFlag
, implicitMain
, meowSearch
, meowResolve
, MeowFileOutput
, MeowFileCallback
, meowrRead
, meowrWrite
, meowrAppend
) where

import Meowscript.Core.AST
import Meowscript.Utils.Types
import Meowscript.Core.StdFiles
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.IORef (newIORef, readIORef, modifyIORef)
import Control.Monad.Reader (asks, liftIO)
import Lens.Micro.Platform (set)
import Meowscript.Utils.IO
import Control.Applicative (liftA2)
import System.FilePath (hasTrailingPathSeparator, (</>), isValid, isAbsolute)

{- Notes:
 - Flags are always lower-case. They're case-insenstiive. 
 - Built-in options keys are always lower-case; their values aren't.
 -
 - Special definitions are case-sensitive.
 - "What is that?" -> These:
 - meowr -specialDef=customValue
 -}

meowState :: FilePathT -> [Text.Text] -> IO ObjectMap -> Maybe MeowCache -> IO MeowState
meowState path args lib cache = return MeowState
    { _meowArgs     = args
    , _meowLib      = lib
    , _meowStd      = stdFiles
    , _meowCache    = cache
    , _meowPath     = path
    , _meowSocket   = Nothing
    , _meowInclude  = []
    , _meowFlags    = Set.empty
    , _meowDefines  = Map.empty }

meowState' :: FilePathT -> [Text.Text] -> IO ObjectMap -> IO MeowState
meowState' path args lib = meowCacheNew >>= meowState path args lib . Just

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
type MeowFileCallback = MeowState -> FilePath -> IO MeowFileOutput

meowResolve :: MeowState -> FilePath -> FilePath
{-# INLINABLE meowResolve #-}
meowResolve state path
    | isDir path && meowHasFlag implicitMain state = path </> "main.meows"
    | otherwise = path
    where isDir = liftA2 (||) hasTrailingPathSeparator (== ".")

meowSearch :: MeowState -> FilePath -> IO (Either Text.Text (MeowState, Text.Text))
{-# INLINABLE meowSearch #-}
meowSearch state path
    | (not . isValid) path = (return . Left) "Invalid filepath."
    | isAbsolute path = fmap (state,) <$> readContents state path
    | otherwise = meowrFile (meowrRead state) paths >>= \case
        Nothing  -> (return . Left) "File does not exist in path!"
        (Just (path', x)) -> case x of
            (Left err) -> return (Left err)
            (Right contents) -> let state' = state { _meowPath = Text.pack path' }
                in return (Right (state', contents))
    where paths = (:) path $ map (</> path) (_meowInclude state)
          readContents = meowFileIO safeReadFile

meowFileIO :: (FilePath -> IO a) -> MeowState -> FilePath -> IO a
{-# INLINABLE meowFileIO #-}
meowFileIO f state = f . meowResolve state

meowrFile :: (FilePath -> IO MeowFileOutput) -> [FilePath] -> IO (Maybe MeowFileOutput)
meowrFile _ [] = return Nothing
meowrFile f (x:xs) = safeDoesFileExist' x >>= \exists -> if exists
    then Just <$> f x
    else meowrFile f xs

meowrRead :: MeowFileCallback
{-# INLINABLE meowrRead #-}
meowrRead state path = readContents state path >>= \case
    (Left exc) -> return (path, Left exc)
    (Right contents) -> return (path, Right contents)
    where readContents = meowFileIO safeReadFile

meowrWrite :: Text.Text -> MeowFileCallback
{-# INLINABLE meowrWrite #-}
meowrWrite contents state path = writeContents state path >>= \case
    (Left exc) -> return (path, Left exc)
    (Right _)  -> return (path, Right Text.empty)
    where writeContents = meowFileIO (safeWriteFile contents)

meowrAppend :: Text.Text -> MeowFileCallback
{-# INLINABLE meowrAppend #-}
meowrAppend contents state path = appendContents state path >>= \case
    (Left exc) -> return (path, Left exc)
    (Right _)  -> return (path, Right Text.empty)
    where appendContents = meowFileIO (safeAppendFile contents)
