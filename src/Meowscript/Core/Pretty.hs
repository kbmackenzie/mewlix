{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Core.Pretty
( showT
, showMeow
, showMeow'
, meowStr
) where

import Meowscript.Core.AST
import Meowscript.Core.Environment
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Functor ((<&>))

showT :: (Show a) => a -> Text.Text
{-# SPECIALISE showT :: Prim -> Text.Text #-}
showT = Text.pack . show 

showMeow :: Prim -> Evaluator Text.Text
showMeow (MeowString x) = return x
showMeow (MeowKey _) = return "<key>"
showMeow (MeowKeys _) = return "<keys>"
showMeow (MeowInt x) = return $ showT x
showMeow (MeowDouble x) = return $ showT x
showMeow (MeowBool x) = return $ if x then "yummy" else "icky"
showMeow MeowLonely = return "lonely"
showMeow (MeowList x) = do 
    prims <- mapM showMeow' x
    let prims' = Text.intercalate ", " prims
    return $ Text.concat [ "[ ", prims', " ]" ]
showMeow (MeowObject x) = do
    let evalPair (key, value) = (evalRef value >>= showMeow') <&> (key,)
    let pretty (key, value) = return $ Text.concat [ key, ": ", value ]
    pairs <- mapM evalPair (Map.toList x) >>= mapM pretty
    return $ Text.concat [ "[ ", Text.intercalate ", " pairs , " ]" ]
showMeow (MeowFunc _ _) = return "<func>"
showMeow (MeowIFunc _ _) = return "<inner-func>"
showMeow (MeowModule _) = return "<module>"

-- Special version that pretty-prints strings,
-- for use in lists and objects.
showMeow' :: Prim -> Evaluator Text.Text
showMeow' (MeowString x) = return $ Text.concat [ "\"", x, "\"" ]
showMeow' x = showMeow x

meowStr :: Prim -> Evaluator Text.Text
{-# INLINE meowStr #-}
meowStr = showMeow'
