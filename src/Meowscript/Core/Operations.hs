{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Operations
( binop
, unop
, binopVar
, unopVar
) where

import Meowscript.Core.AST
import Meowscript.Core.Keys
import Meowscript.Core.Primitives
import Meowscript.Core.Exceptions
import Meowscript.Core.Pretty (showMeow)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Control.Monad.Except (throwError)
import Control.Monad (join)
import Data.Functor ((<&>))
import Data.Fixed (mod')

{- Binary Operations -}
-------------------------------------------------------------------------
binop :: Binop -> Prim -> Prim -> Evaluator Prim
{-# INLINABLE binop #-}
binop MeowPush = meowPush
binop MeowAssign = meowAssign
binop op = binopVar $ case op of
    MeowAdd -> meowAdd
    MeowSub -> meowSub
    MeowMul -> meowMul
    MeowDiv -> meowDiv
    MeowMod -> meowMod
    (MeowCompare ord) -> meowCompare ord
    MeowConcat -> meowConcat
    MeowPow -> meowPow

{- Unary Operations -}
-------------------------------------------------------------------------
unop :: Unop -> Prim -> Evaluator Prim
{-# INLINABLE unop #-}
unop MeowPaw = meowPaw
unop MeowClaw = meowClaw
unop MeowKnockOver = meowKnock
unop op = unopVar $ case op of
    MeowLen -> meowLen
    MeowNegate -> meowNegate
    MeowNot -> meowNot
    MeowPeek -> meowPeek


{- Variables -}
-------------------------------------------------------------------------
binopVar :: (Prim -> Prim -> Evaluator Prim) -> Prim -> Prim -> Evaluator Prim
{-# INLINABLE binopVar #-}
binopVar f x y = join (f <$> ensureValue x <*> ensureValue y)

unopVar :: (Prim -> Evaluator Prim) -> Prim -> Evaluator Prim
{-# INLINABLE unopVar #-}
unopVar f x = ensureValue x >>= f

-- All of these operations assume a key is already in its most reduced form.
-- A key should never point to another key!


{- Assignment Operations -}
-------------------------------------------------------------------------
meowAssign :: Prim -> Prim -> Evaluator Prim
meowAssign (MeowKey a) value = do
    value' <- ensureValue value
    assignment a value'
    return value'
meowAssign a b = throwError =<< opException "assignment" [a, b]

-- Paw (++)
meowPaw :: Prim -> Evaluator Prim
meowPaw (MeowKey a) = do
    num <- keyLookup a >>= meowPaw
    assignment a num
    return num
meowPaw (MeowInt a) = (return . MeowInt . succ) a
meowPaw (MeowDouble a) = (return . MeowDouble . succ) a
meowPaw a = throwError =<< opException "paw" [a]

-- Claw (--)
meowClaw :: Prim -> Evaluator Prim
meowClaw (MeowKey a) = do
    num <- keyLookup a >>= meowClaw
    assignment a num
    return num
meowClaw (MeowInt a) = (return . MeowInt . pred) a
meowClaw (MeowDouble a) = (return . MeowDouble . pred) a
meowClaw a = throwError =<< opException "claw" [a]


{- List Operations -}
-------------------------------------------------------------------------
-- Push
meowPush :: Prim -> Prim -> Evaluator Prim
meowPush a (MeowKey b) = do
    a' <- ensureValue a
    list <- keyLookup b >>= meowPush' a'
    assignment b list
    return list
meowPush a (MeowList b) = ensureValue a <&> MeowList . (:b)
meowPush a b = throwError =<< opException "push" [a, b]

meowPush' :: Prim -> Prim -> Evaluator Prim
meowPush' a (MeowList b) = (return . MeowList) (a:b)
meowPush' a (MeowString b) = showMeow a >>= \x -> (return . MeowString) (x `Text.append` b)
meowPush' a b = throwError =<< opException "push" [a, b]

-- Knock over
meowKnock :: Prim -> Evaluator Prim
meowKnock (MeowKey a) = do
    list <- keyLookup a >>= meowKnock'
    assignment a list
    return list
meowKnock (MeowList a) = (return . MeowList) $ case a of
    [] -> []
    (_:xs) -> xs
meowKnock a = throwError =<< opException "knock over" [a]

meowKnock' :: Prim -> Evaluator Prim
meowKnock' (MeowList a) = (return . MeowList) (if null a then a else tail a)
meowKnock' (MeowString a) = (return . MeowString) (if Text.null a then a else Text.tail a)
meowKnock' a = throwError =<< opException "knock over" [a]
 

{- Arithmetic Operations -}
-------------------------------------------------------------------------
-- Addition
meowAdd :: Prim -> Prim -> Evaluator Prim
meowAdd (MeowInt a) (MeowInt b) = (return . MeowInt) (a + b)
meowAdd (MeowInt a) (MeowDouble b) = (return . MeowDouble) (fromIntegral a + b)
meowAdd (MeowDouble a) (MeowInt b) = (return . MeowDouble) (a + fromIntegral b)
meowAdd (MeowDouble a) (MeowDouble b) = (return . MeowDouble) (a + b)
meowAdd x y = throwError =<< opException "+" [x, y]

-- Negation
meowNegate :: Prim -> Evaluator Prim
meowNegate (MeowInt a) = (return . MeowInt . negate) a
meowNegate (MeowDouble a) = (return . MeowDouble . negate) a
meowNegate a = throwError =<< opException "-" [a]

-- Subtraction
meowSub :: Prim -> Prim -> Evaluator Prim
meowSub x y = meowNegate y >>= meowAdd x

-- Multiplication
meowMul :: Prim -> Prim -> Evaluator Prim
meowMul (MeowInt a) (MeowInt b) = (return . MeowInt) (a * b)
meowMul (MeowInt a) (MeowDouble b) = (return . MeowDouble) (fromIntegral a * b)
meowMul (MeowDouble a) (MeowInt b) = (return . MeowDouble) (a * fromIntegral b)
meowMul (MeowDouble a) (MeowDouble b) = (return . MeowDouble) (a * b)
meowMul x y = throwError =<< opException "*" [x, y]

-- Division
meowDiv :: Prim -> Prim -> Evaluator Prim
meowDiv a b@(MeowInt 0) = throwError =<< divByZero [a, b]
meowDiv a b@(MeowDouble 0) = throwError =<< divByZero [a, b]
meowDiv (MeowInt a) (MeowInt b) = (return . MeowDouble) (fromIntegral a / fromIntegral b)
meowDiv (MeowInt a) (MeowDouble b) = (return . MeowDouble) (fromIntegral a / b)
meowDiv (MeowDouble a) (MeowInt b) = (return . MeowDouble) (a / fromIntegral b)
meowDiv (MeowDouble a) (MeowDouble b) = (return . MeowDouble) (a / b)
meowDiv x y = throwError =<< opException "/" [x, y]

-- Modulo
meowMod :: Prim -> Prim -> Evaluator Prim
meowMod a b@(MeowInt 0) = throwError =<< divByZero [a, b]
meowMod a b@(MeowDouble 0) = throwError =<< divByZero [a, b]
meowMod (MeowInt a) (MeowInt b) = (return . MeowInt) (a `mod` b)
meowMod (MeowInt a) (MeowDouble b) = (return . MeowDouble) (fromIntegral a `mod'` b)
meowMod (MeowDouble a) (MeowInt b) = (return . MeowDouble) (a `mod'` fromIntegral b)
meowMod (MeowDouble a) (MeowDouble b) = (return . MeowDouble) (a `mod'` b)
meowMod x y = throwError =<< opException "%" [x, y]

-- Power
meowPow :: Prim -> Prim -> Evaluator Prim
meowPow (MeowInt a) (MeowInt b) = (return . MeowDouble) (fromIntegral a ^ b)
meowPow (MeowDouble a) (MeowInt b) = (return . MeowDouble) (a ^ b)
meowPow x y = throwError =<< opException "**" [x, y]


-- Comparison
meowCompare :: [Ordering] -> Prim -> Prim -> Evaluator Prim
meowCompare ord a b = MeowBool . (`elem` ord) <$> (a `primCompare` b)


{-- Logical Operations --}
-------------------------------------------------------------------------
-- Not
meowNot :: Prim -> Evaluator Prim
meowNot = return . MeowBool . not . meowBool


{- String/List Manipulation -}
-------------------------------------------------------------------------
-- Length 
meowLen :: Prim -> Evaluator Prim
meowLen (MeowString a) = (return . MeowInt . Text.length) a
meowLen (MeowList a) = (return . MeowInt . length) a
meowLen (MeowObject a) = (return . MeowInt . Map.size) a
meowLen x = throwError =<< opException "length (?!)" [x]

-- Concat
meowConcat :: Prim -> Prim -> Evaluator Prim
meowConcat (MeowString a) (MeowString b) = (return . MeowString) (Text.append a b)
meowConcat (MeowList a) (MeowList b) = (return . MeowList) (a ++ b)
meowConcat (MeowObject a) (MeowObject b) = (return . MeowObject) (a <> b)
meowConcat a b = MeowString <$> (Text.append <$> showMeow a <*> showMeow b)

-- Peek
meowPeek :: Prim -> Evaluator Prim
meowPeek (MeowList a) = return (if null a then MeowLonely else head a)
meowPeek (MeowString a) = (return . MeowString) res
    where res = if Text.null a then a else (Text.pack . (: []) . Text.head) a
meowPeek a = throwError =<< opException "peek" [a]
