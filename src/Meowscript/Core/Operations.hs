{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Operations
( binop
, unop
, binopVar
, unopVar
) where

import Meowscript.Core.AST
import Meowscript.Core.Keys
import Meowscript.Core.Exceptions
import Meowscript.Core.Pretty (showMeow)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.Except (throwError)
import Control.Monad (join)

{- Binary Operations -}
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
    MeowAnd -> meowAnd
    MeowOr -> meowOr
    MeowConcat -> meowConcat

{- Unary Operations -}
unop :: Unop -> Prim -> Evaluator Prim
{-# INLINABLE unop #-}
unop MeowPaw = meowPaw
unop MeowClaw = meowClaw
unop MeowKnockOver = meowKnock
unop op = unopVar $ case op of
--    MeowYarn -> meowYarn
    MeowLen -> meowLen
    MeowNegate -> meowNegate
    MeowNot -> meowNot
    MeowPeek -> meowPeek

{- Variables -}
binopVar :: (Prim -> Prim -> Evaluator Prim) -> Prim -> Prim -> Evaluator Prim
{-# INLINABLE binopVar #-}
binopVar f x y = join (f <$> ensureValue x <*> ensureValue y)

unopVar :: (Prim -> Evaluator Prim) -> Prim -> Evaluator Prim
{-# INLINABLE unopVar #-}
unopVar f x = ensureValue x >>= f

-- All of these operations assume a key is already in its most reduced form.
-- A key should never point to another key!

{- Assignment Operations -}
meowAssign :: Prim -> Prim -> Evaluator Prim
meowAssign (MeowKey a) value = do
    value' <- ensureValue value
    assignment a value'
    return value'
meowAssign a b = throwError =<< opException "assignment" [a, b]

-- Paw (++)
meowPaw :: Prim -> Evaluator Prim
meowPaw (MeowKey a) = do
    num <- keyLookup a >>= meowPaw'
    assignment a num
    return num
meowPaw a = throwError =<< opException "paw" [a]

meowPaw' :: Prim -> Evaluator Prim
meowPaw' (MeowInt a) = (return . MeowInt . succ) a
meowPaw' (MeowDouble a) = (return . MeowDouble . succ) a
meowPaw' a = throwError =<< opException "paw" [a]

-- Claw (--)
meowClaw :: Prim -> Evaluator Prim
meowClaw (MeowKey a) = do
    num <- keyLookup a >>= meowClaw'
    assignment a num
    return num
meowClaw a = throwError =<< opException "claw" [a]

meowClaw' :: Prim -> Evaluator Prim
meowClaw' (MeowInt a) = (return . MeowInt . pred) a
meowClaw' (MeowDouble a) = (return . MeowDouble . pred) a
meowClaw' a = throwError =<< opException "claw" [a]


{- List Assignment -}

-- Push
meowPush :: Prim -> Prim -> Evaluator Prim
meowPush a (MeowKey b) = do
    a' <- ensureValue a
    list <- keyLookup b >>= meowPush' a'
    assignment b list
    return list
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
meowKnock a = throwError =<< opException "knock over" [a]
 

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
meowMod x y = throwError (opException "%" [x, y])


-- Comparison
meowCompare :: [Ordering] -> Prim -> Prim -> Evaluator Prim
meowCompare ord a b = (return . MeowBool) (c `elem` ord)
    where c = a `compare` b


{-- Logical Operations --}

-- Not
meowNot :: Prim -> Evaluator Prim
meowNot = return . MeowBool . not . meowBool

-- And
meowAnd :: Prim -> Prim -> Evaluator Prim
meowAnd x y = (return . MeowBool) (meowBool x && meowBool y)

-- Or
meowOr :: Prim -> Prim -> Evaluator Prim
meowOr x y = (return . MeowBool) (meowBool x || meowBool y)



{- String/List Manipulation -}

{-
-- Yarn
meowYarn :: Prim -> Evaluator Prim
meowYarn (MeowString a) = (return . MeowKey . KeyModify) a
meowYarn x = throwError (opException "~~" [x])
-}

-- Length 
meowLen :: Prim -> Evaluator Prim
meowLen (MeowString a) = (return . MeowInt . Text.length) a
meowLen (MeowList a) = (return . MeowInt . length) a
meowLen (MeowObject a) = (return . MeowInt . Map.size) a
meowLen x = throwError =<< opException "?" [x]

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



{- List Assignment -}
   
meowKnock' :: Prim -> Evaluator Prim
meowKnock' (MeowList a) = (return . MeowList) (if null a then a else tail a)
meowKnock' (MeowString a) = (return . MeowString) (if Text.null a then a else Text.tail a)
meowKnock' a = throwError =<< opException "knock over" [a]
