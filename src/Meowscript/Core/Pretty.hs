{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.Pretty
( showT
, showMeow
, prettyMeow
) where

import Meowscript.Core.AST
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.Reader (liftIO)
import Data.Functor ((<&>))
import Data.IORef (readIORef)

showT :: (Show a) => a -> Text.Text
{-# INLINABLE showT #-}
{-# SPECIALISE showT :: Prim -> Text.Text #-}
showT = Text.pack . show 

-- Defining it here to avoid cyclical dependencies. c':
-- This way, 'Environment.hs' can reference this file.
evalRef :: PrimRef -> Evaluator Prim
{-# INLINABLE evalRef #-}
evalRef = liftIO . readIORef

showMeow :: Prim -> Evaluator Text.Text
{-# INLINABLE showMeow #-}
showMeow (MeowString x) = return x
showMeow (MeowKey x) = return $ Text.concat ["<key: \"", showT x, "\">" ]
showMeow (MeowInt x) = return $ showT x
showMeow (MeowDouble x) = return $ showT x
showMeow (MeowBool x) = return $ if x then "yummy" else "icky"
showMeow MeowLonely = return "lonely"
showMeow (MeowList x) = do 
    prims <- mapM prettyMeow x
    let prims' = Text.intercalate ", " prims
    return $ Text.concat [ "[ ", prims', " ]" ]
showMeow (MeowObject x) = do
    let evalPair (key, value) = (evalRef value >>= prettyMeow) <&> (key,)
    let pretty (key, value) = return $ Text.concat [ key, ": ", value ]
    pairs <- mapM evalPair (Map.toList x) >>= mapM pretty
    return $ Text.concat [ "[ ", Text.intercalate ", " pairs , " ]" ]
showMeow (MeowFunc {}) = return "<func>"
showMeow (MeowIFunc _ _) = return "<inner-func>"

-- Special version that pretty-prints strings,
-- for use in lists and objects.
prettyMeow :: Prim -> Evaluator Text.Text
{-# INLINE prettyMeow #-}
prettyMeow (MeowString x) = return $ Text.concat [ "\"", x, "\"" ]
prettyMeow x = showMeow x
