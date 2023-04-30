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

-- Inline pragmas.
{-# INLINE ensureValid #-}
{-# INLINE binop #-}
{-# INLINE unop #-}
{-# INLINE binopVar #-}
{-# INLINE unopVar #-}

{- Binary Operations -}
binop :: Binop -> Prim -> Prim -> Evaluator Prim

binop MeowAssign a (MeowTrail b) = lookUpTrail b >>= binop MeowAssign a
binop MeowAssign a (MeowKey b) = lookUpVar b >>= binop MeowAssign a
binop MeowAssign (MeowTrail a) b = insertWithTrail a b >> return b
binop MeowAssign (MeowKey a) b = insertVar a b >> return b
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
            _ -> undefined --todo

{- Unary Operations -}
unop :: Unop -> Prim -> Evaluator Prim
unop op = unopVar fn
    where fn = case op of
            MeowYarn -> meowYarn
            MeowLen -> meowLen
            MeowPoke -> meowPoke
            MeowNudge -> meowNudge
            MeowNegate -> meowNegate
            MeowNot -> meowNot
            MeowPeek -> meowPeek
            MeowSneak -> meowSneak


{- Variables -}

-- Wrappers for evaluating variables.
-- This way, operations don't have to worry about this.
ensureValid :: Prim -> Evaluator Prim
ensureValid (MeowKey x) = lookUpVar x >>= ensureValid
ensureValid (MeowTrail xs) = lookUpTrail xs >>= ensureValid
ensureValid x = return x

binopVar :: (Prim -> Prim -> Evaluator Prim) -> Prim -> Prim -> Evaluator Prim
binopVar f x y = do
    x' <- ensureValid x
    y' <- ensureValid y
    f x' y'

unopVar :: (Prim -> Evaluator Prim) -> Prim -> Evaluator Prim
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
meowYarn x = throwError $ yarnError x

yarnError :: Prim -> Text.Text
yarnError = unopError "Yarn" "~~"


-- YarnLen
meowLen :: Prim -> Evaluator Prim

meowLen (MeowString a) = (return . MeowInt . Text.length) a
meowLen (MeowList a) = (return . MeowInt . length) a
meowLen (MeowObject a) = (return . MeowInt . Map.size) a
meowLen x = throwError $ lenError x

lenError :: Prim -> Text.Text
lenError = unopError "Len" "~?"


-- Concat
meowConcat :: Prim -> Prim -> Evaluator Prim

meowConcat (MeowList a) (MeowList b) = (return . MeowList) (a ++ b)
meowConcat a b = (return . MeowString) ab
    where ab = Text.append (asString a) (asString b)


-- Poke
meowPoke :: Prim -> Evaluator Prim
meowPoke (MeowList a) = (return . MeowList) (if null a then a else tail a)
meowPoke a = (return . MeowString) res
    where a' = asString a
          res = if Text.null a' then a' else Text.tail a'

-- Nudge
meowNudge :: Prim -> Evaluator Prim
meowNudge (MeowList a) = (return . MeowList) (if null a then a else init a)
meowNudge a = (return . MeowString) res
    where a' = asString a
          res = if Text.null a' then a' else Text.init a'

-- Peek
meowPeek :: Prim -> Evaluator Prim
meowPeek (MeowList a) = return (if null a then MeowLonely else head a)
meowPeek a = (return . MeowString) res
    where a' = asString a
          res = if Text.null a' then a' else (Text.pack . (: []) . Text.head) a'

-- Sneak
meowSneak :: Prim -> Evaluator Prim
meowSneak (MeowList a) = return (if null a then MeowLonely else last a)
meowSneak a = (return . MeowString) res
    where a' = asString a
          res = if Text.null a' then a' else (Text.pack . (: []) . Text.last) a'
