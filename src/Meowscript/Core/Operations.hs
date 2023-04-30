{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Operations
( binop
, unop
, binopVar
, unopVar
) where

import Meowscript.Core.AST
import Meowscript.Core.Evaluate
import Meowscript.Core.Environment
import Meowscript.Core.Messages
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.Except (throwError)

{- Binary Operations -}
binop :: Binop -> Prim -> Prim -> Evaluator Prim
{-# INLINE binop #-}
binop MeowAssign a (MeowTrail b) = lookUpTrail b >>= binop MeowAssign a
binop MeowAssign a (MeowKey b)   = lookUpVar b >>= binop MeowAssign a
binop MeowAssign (MeowTrail a) b = insertWithTrail a b >> return b
binop MeowAssign (MeowKey a) b   = insertVar a b >> return b
binop MeowAssign a b = throwError (binopError "Invalid assignment!" "=" a b)
binop op a b = binopVar fn a b
    where fn = case op of
            MeowAdd -> meowAdd
            MeowSub -> meowSub
            MeowMul -> meowMul
            MeowDiv -> meowDiv
            (MeowCompare ord) -> meowCompare ord
            MeowAnd -> meowAnd
            MeowOr -> meowOr
            MeowConcat -> meowConcat
            MeowPush -> meowPush
            _ -> undefined -- This should never be matched, right?

{- Unary Operations -}
unop :: Unop -> Prim -> Evaluator Prim
{-# INLINE unop #-}
unop op = unopVar fn
    where fn = case op of
            MeowYarn -> meowYarn
            MeowLen -> meowLen
            MeowNegate -> meowNegate
            MeowNot -> meowNot
            MeowPeek -> meowPeek
            MeowKnockOver -> meowKnock

{- Variables -}

-- Ensures a value is a concrete value (and not a key nor trail).
ensureValue :: Prim -> Evaluator Prim
{-# INLINE ensureValue #-}
ensureValue (MeowKey x) = lookUpVar x >>= ensureValue
ensureValue (MeowTrail xs) = lookUpTrail xs >>= ensureValue
ensureValue x = return x

binopVar :: (Prim -> Prim -> Evaluator Prim) -> Prim -> Prim -> Evaluator Prim
{-# INLINE binopVar #-}
binopVar f x y = do
    x' <- ensureValue x
    y' <- ensureValue y
    f x' y'
unopVar :: (Prim -> Evaluator Prim) -> Prim -> Evaluator Prim
{-# INLINE unopVar #-}
unopVar f (MeowKey a) = lookUpVar a >>= f
unopVar f x = f x


{- TO DO -}
-- Better error messages.

-- Addition
meowAdd :: Prim -> Prim -> Evaluator Prim
meowAdd x@(MeowInt a) y = case y of
    (MeowInt b) -> return $ MeowInt (a + b)
    (MeowDouble b) -> return $ MeowDouble (fromIntegral a + b)
    _ -> throwError $ addError x y

meowAdd x@(MeowDouble a) y = case y of
    (MeowInt b) -> return $ MeowDouble (a + fromIntegral b)
    (MeowDouble b) -> return $ MeowDouble (a + b)
    _ -> throwError $ addError x y

meowAdd x y = throwError $ addError x y

-- Addition error logging.
addError :: Prim -> Prim -> Text.Text
addError = binopError "Addition" "+"


-- Negation
meowNegate :: Prim -> Evaluator Prim
meowNegate (MeowInt a) = return $ MeowInt (negate a)
meowNegate (MeowDouble a) = return $ MeowDouble (negate a)
meowNegate x = throwError $ unopError "Negation" "-" x


-- Subtraction
meowSub :: Prim -> Prim -> Evaluator Prim
meowSub x y = meowNegate y >>= meowAdd x


-- Multiplication
meowMul :: Prim -> Prim -> Evaluator Prim

meowMul x@(MeowInt a) y = case y of
    (MeowInt b) -> return $ MeowInt (a * b)
    (MeowDouble b) -> return $ MeowDouble (fromIntegral a * b)
    _ -> throwError $ mulError x y
meowMul x@(MeowDouble a) y = case y of
    (MeowInt b) -> return $ MeowDouble (a * fromIntegral b)
    (MeowDouble b) -> return $ MeowDouble (a * b)
    _ -> throwError $ mulError x y
meowMul x y = throwError $ mulError x y

-- Multiplication errors.
mulError :: Prim -> Prim -> Text.Text
mulError = binopError "Multiplication" "*"


-- Division
meowDiv :: Prim -> Prim -> Evaluator Prim

meowDiv x@(MeowInt a) y = case y of
    (MeowInt 0) -> throwError $ divByZero x y
    (MeowInt b) -> return $ MeowInt (a `div` b)
    (MeowDouble 0) -> throwError $ divByZero x y
    (MeowDouble b) -> return $ MeowDouble (fromIntegral a / b)
    _ -> throwError $ divError x y
meowDiv x@(MeowDouble a) y = case y of
    (MeowInt 0) -> throwError $ divByZero x y
    (MeowInt b) -> return $ MeowDouble (a / fromIntegral b)
    (MeowDouble 0) -> throwError $ divByZero x y
    (MeowDouble b) -> return $ MeowDouble (a / b)
    _ -> throwError $ divError x y
meowDiv x y = throwError $ divError x y

-- Division exceptions.
divError :: Prim -> Prim -> Text.Text

divError = binopError "Division" "/"
divByZero :: Prim -> Prim -> Text.Text
divByZero x y = Text.intercalate "\n" [ divError x y, "Cannot divide by zero!" ]


-- Comparison
meowCompare :: [Ordering] -> Prim -> Prim -> Evaluator Prim

meowCompare ord a b = (return . MeowBool) (c `elem` ord)
    where c = a `compare` b


{-- Logical Operations --}

-- Not
meowNot :: Prim -> Evaluator Prim
meowNot = return . MeowBool . not . asBool

-- And
meowAnd :: Prim -> Prim -> Evaluator Prim
meowAnd x y = (return . MeowBool) (asBool x && asBool y)

-- Or
meowOr :: Prim -> Prim -> Evaluator Prim
meowOr x y = (return . MeowBool) (asBool x || asBool y)



{- String/List Manipulation -}

-- Yarn
meowYarn :: Prim -> Evaluator Prim

meowYarn (MeowString a) = return (MeowKey a)
meowYarn x = throwError (unopError "yarn" "~~" x)


-- YarnLen
meowLen :: Prim -> Evaluator Prim

meowLen (MeowString a) = (return . MeowInt . Text.length) a
meowLen (MeowList a) = (return . MeowInt . length) a
meowLen (MeowObject a) = (return . MeowInt . Map.size) a
meowLen x = throwError (unopError "len" "~?" x)


-- Concat
meowConcat :: Prim -> Prim -> Evaluator Prim

meowConcat (MeowString a) (MeowString b) = (return . MeowString) (Text.append a b)
meowConcat (MeowList a) (MeowList b) = (return . MeowList) (a ++ b)
meowConcat (MeowObject a) (MeowObject b) = (return . MeowObject) (a <> b)
meowConcat a b = (return . MeowString) ab
    where ab = Text.append (asString a) (asString b)


-- Push
meowPush :: Prim -> Prim -> Evaluator Prim
meowPush (MeowList a) b = (return . MeowList) (b:a)
meowPush (MeowString a) b = (return . MeowString) (asString b `Text.append` a)
meowPush a b = throwError (binopError "Push" "push" a b)

-- Peek
meowPeek :: Prim -> Evaluator Prim
meowPeek (MeowList a) = return (if null a then MeowLonely else head a)
meowPeek a = (return . MeowString) res
    where a' = asString a
          res = if Text.null a' then a' else (Text.pack . (: []) . Text.head) a'

-- Knock over
meowKnock :: Prim -> Evaluator Prim
meowKnock (MeowList a) = (return . MeowList) (if null a then a else tail a)
meowKnock (MeowString a) = (return . MeowString) (if Text.null a then a else Text.tail a)
meowKnock a = throwError (unopError "knock over" "knock over" a)
