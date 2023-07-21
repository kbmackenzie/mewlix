{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Core.MeowState
( meowState
, meowState'
, meowSetPath
, cacheAdd
, cacheLookup
, meowCacheNew
, meowHasFlag
, implicitMain
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

{- Notes:
 - Flags are always lower-case. They're case-insenstiive. 
 - Options keys are always lower-case; their values aren't.
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
