{-# LANGUAGE OverloadedStrings #-} 

module Meowscript.Core.Operations
( binop
, unop
, binopVar
, unopVar
, asBool
, asString
) where

import Meowscript.Core.AST
import Meowscript.Core.Evaluate
import Meowscript.Core.Environment
import Meowscript.Core.Messages
import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.State (get, put, liftIO)
import Control.Monad.Except (throwError)

{- Binary Operations -}
binop :: Binop -> Prim -> Prim -> Evaluator Prim

binop MeowAssign a (MeowAtom b) = do
    value <- lookUpVar b
    binop MeowAssign a value
binop MeowAssign (MeowAtom a) b = do
    insertVar a b
    return b
binop MeowAssign a b = throwError (binopError "Invalid assignment!" "" a b)
    
binop op a b =
    let fn = case op of
            MeowAdd -> meowAdd
            MeowSub -> meowSub
            MeowMul -> meowMul
            MeowDiv -> meowDiv
            (MeowCompare ord) -> meowCompare ord
            MeowAnd -> meowAnd
            MeowOr -> meowOr
            MeowConcat -> meowConcat
    in binopVar fn a b
    

{- Unary Operations -}
unop :: Unop -> Prim -> Evaluator Prim
unop op a =
    let fn = case op of
            MeowYarn -> meowYarn
            MeowLen -> meowLen
            MeowPoke -> meowPoke
            MeowNudge -> meowNudge
            MeowNegate -> meowNegate
            MeowNot -> meowNot
            MeowPeek -> meowPeek
            MeowSneak -> meowSneak
    in unopVar fn a


{- Variables -}

{- Wrappers for evaluating variables.
 - This way, operations don't have to worry about this. -}
binopVar :: (Prim -> Prim -> Evaluator Prim) -> Prim -> Prim -> Evaluator Prim
binopVar f (MeowAtom a) y = do
    x <- lookUpVar a
    binopVar f x y
binopVar f x (MeowAtom b) = do
    y <- lookUpVar b
    binopVar f x y
binopVar f x y = f x y

unopVar :: (Prim -> Evaluator Prim) -> Prim -> Evaluator Prim
unopVar f (MeowAtom a) = do
    x <- lookUpVar a
    f x
unopVar f x = f x



{- TO DO -}
-- Better error messages.

{- Addition -}
meowAdd :: Prim -> Prim -> Evaluator Prim

meowAdd x@(MeowInt a) y = case y of
    (MeowInt b) -> return $ MeowInt (a + b)
    (MeowDouble b) -> let a' = fromIntegral a
                      in return $ MeowDouble (a' + b)
    _ -> throwError $ addError x y

meowAdd x@(MeowDouble a) y = case y of
    (MeowInt b) -> let b' = fromIntegral b
                   in return $ MeowDouble (a + b')
    (MeowDouble b) -> return $ MeowDouble (a + b)
    _ -> throwError $ addError x y

meowAdd x y = throwError $ addError x y

addError :: Prim -> Prim -> Text.Text
addError = binopError "Addition" "+"


{- Negation -}
meowNegate :: Prim -> Evaluator Prim

meowNegate (MeowInt a) = return $ MeowInt (negate a)
meowNegate (MeowDouble a) = return $ MeowDouble (negate a)
meowNegate x = throwError $ unopError "Negation" "-" x


{- Subtraction -}
meowSub :: Prim -> Prim -> Evaluator Prim

meowSub x y = do
    y' <- meowNegate y
    meowAdd x y'


{- Multiplication -}
meowMul :: Prim -> Prim -> Evaluator Prim

meowMul x@(MeowInt a) y = case y of
    (MeowInt b) -> return $ MeowInt (a * b)
    (MeowDouble b) -> let a' = fromIntegral a
                      in return $ MeowDouble (a' * b)
    _ -> throwError $ mulError x y

meowMul x@(MeowDouble a) y = case y of
    (MeowInt b) -> let b' = fromIntegral b
                   in return $ MeowDouble (a * b')
    (MeowDouble b) -> return $ MeowDouble (a * b)
    _ -> throwError $ mulError x y

meowMul x y = throwError $ mulError x y

mulError :: Prim -> Prim -> Text.Text
mulError = binopError "Multiplication" "*"


{- Division -}
meowDiv :: Prim -> Prim -> Evaluator Prim

meowDiv x@(MeowInt a) y = case y of
    (MeowInt 0) -> throwError $ divByZero x y
    (MeowInt b) -> return $ MeowInt (a `div` b)
    (MeowDouble 0) -> throwError $ divByZero x y
    (MeowDouble b) -> let a' = fromIntegral a
                      in return $ MeowDouble (a' / b)
    _ -> throwError $ divError x y

meowDiv x@(MeowDouble a) y = case y of
    (MeowInt 0) -> throwError $ divByZero x y
    (MeowInt b) -> let b' = fromIntegral b
                   in return $ MeowDouble (a / b')
    (MeowDouble 0) -> throwError $ divByZero x y
    (MeowDouble b) -> return $ MeowDouble (a / b)
    _ -> throwError $ divError x y

meowDiv x y = throwError $ divError x y

divError :: Prim -> Prim -> Text.Text
divError = binopError "Division" "/"

divByZero :: Prim -> Prim -> Text.Text
divByZero x y = Text.intercalate "\n" [ divError x y, "Cannot divide by zero!" ]


{- Comparison -}
meowCompare :: [Ordering] -> Prim -> Prim -> Evaluator Prim
meowCompare ord a b = let c = a `compare` b
                      in return $ MeowBool (c `elem` ord)


{- Logical Operations -}

{- Helper -}
asBool :: Prim -> Bool

asBool (MeowBool a) = a
asBool MeowLonely = False
asBool (MeowString "") = False
asBool _ = True

{- Not -}
meowNot :: Prim -> Evaluator Prim
meowNot x = let x' = asBool x
            in return $ MeowBool (not x')

{- And -}
meowAnd :: Prim -> Prim -> Evaluator Prim
meowAnd x y = return $ MeowBool (x' && y')
    where x' = asBool x
          y' = asBool y


{- Or -}
meowOr :: Prim -> Prim -> Evaluator Prim
meowOr x y = return $ MeowBool (x' || y')
    where x' = asBool x
          y' = asBool y



{- String Manipulation -}

{- Yarn -}
meowYarn :: Prim -> Evaluator Prim

meowYarn (MeowString a) = return (MeowAtom a)
meowYarn x = throwError $ yarnError x

yarnError :: Prim -> Text.Text
yarnError = unopError "Yarn" "~~"


{- YarnLen -}
meowLen :: Prim -> Evaluator Prim

meowLen (MeowString a) = let len = Text.length a
                         in return (MeowInt len)
meowLen x = throwError $ lenError x

lenError :: Prim -> Text.Text
lenError = unopError "Len" "~?"


{- Stringify -}
asString :: Prim -> Text.Text

asString (MeowString a) = a
asString x = showT x


{- Concat -}
meowConcat :: Prim -> Prim -> Evaluator Prim

meowConcat x y = let a = asString x
                     b = asString y
                     ab = Text.append a b
                 in (return . MeowString) ab

{- Poke -}
meowPoke :: Prim -> Evaluator Prim
meowPoke x = let a = asString x
                 a' = if Text.null a then a else Text.tail a
             in (return . MeowString) a'

{- Nudge -}
meowNudge :: Prim -> Evaluator Prim
meowNudge x = let a = asString x
                  a' = if Text.null a then a else Text.init a
              in (return . MeowString) a'

{- Peek -}
meowPeek :: Prim -> Evaluator Prim
meowPeek x = let a = asString x
                 a' = if Text.null a then a
                      else (Text.pack . (: []) . Text.head) a
             in (return . MeowString) a'


{- Sneak -}
meowSneak :: Prim -> Evaluator Prim
meowSneak x = let a = asString x
                  a' = if Text.null a then a
                       else (Text.pack . (: []) . Text.last) a
              in (return . MeowString) a'
