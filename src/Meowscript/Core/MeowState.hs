{-# LANGUAGE LambdaCase #-}

module Meowscript.Core.MeowState
( meowState
, meowState'
, meowNewPath
, cacheAdd
, cacheLookup
, meowCacheNew
) where

import Meowscript.Core.AST
import Meowscript.Utils.Types
import Meowscript.Core.StdFiles
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Data.IORef (newIORef, readIORef, modifyIORef)
import Control.Monad.Reader (asks, liftIO)

meowState :: FilePathT -> [Text.Text] -> IO ObjectMap -> Maybe MeowCache -> IO MeowState
meowState path args lib cache = return MeowState
    { meowArgs   = args
    , meowLib    = lib
    , meowStd    = stdFiles
    , meowCache  = cache
    , meowPath   = path
    , meowSocket = Nothing }

meowState' :: FilePathT -> [Text.Text] -> IO ObjectMap -> IO MeowState
meowState' path args lib = meowCacheNew >>= meowState path args lib . Just

meowNewPath :: MeowState -> FilePathT -> MeowState
meowNewPath (MeowState args lib std cache _ socket) = flip (MeowState args lib std cache) socket

cacheAdd :: FilePathT -> Environment -> Evaluator ()
cacheAdd path env = asks (meowCache . fst) >>= \case
    Nothing -> return ()
    (Just cache) -> liftIO $ modifyIORef cache (Map.insert path env)

cacheLookup :: FilePathT -> Evaluator (Maybe Environment)
cacheLookup path = asks (meowCache . fst) >>= \case
    Nothing -> return Nothing
    (Just cache) -> Map.lookup path <$> (liftIO . readIORef) cache

meowCacheNew :: IO MeowCache
meowCacheNew = newIORef Map.empty
