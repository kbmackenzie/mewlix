{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Meowscript.Interpreter.Operations
( meowAdd
, meowSub
, meowMul
, meowDiv
, meowMod
, meowPow
, meowNegate
, meowNot
, meowEq
, meowLesser
, meowGreater
, meowLEQ
, meowGEQ
, meowNotEq
, meowPush
, meowPeek
, meowPop
, meowConcat
, meowLength
) where

import Meowscript.Abstract.Meow
import Meowscript.Data.Ref
import Meowscript.Abstract.Meowable
import Meowscript.Abstract.Prettify
import Meowscript.Interpreter.Primitive
import Meowscript.Interpreter.Exceptions
import qualified Data.Text as Text
import qualified Meowscript.Data.Stack as Stack
import qualified Data.HashMap.Strict as HashMap
import Data.Fixed (mod')

{- Arithmetic Operations -}
---------------------------------------------------------------------------------
meowAdd :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
MeowInt a   `meowAdd` MeowInt b     = (return . MeowInt) (a + b)
MeowInt a   `meowAdd` MeowFloat b   = (return . MeowFloat) (fromIntegral a + b)
MeowFloat a `meowAdd` MeowInt b     = (return . MeowFloat) (a + fromIntegral b)
MeowFloat a `meowAdd` MeowFloat b   = (return . MeowFloat) (a + b)
a `meowAdd` b = throwError =<< operationException "addition" [a, b]

meowSub :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
MeowInt a   `meowSub` MeowInt b     = (return . MeowInt) (a - b)
MeowInt a   `meowSub` MeowFloat b   = (return . MeowFloat) (fromIntegral a - b)
MeowFloat a `meowSub` MeowInt b     = (return . MeowFloat) (a - fromIntegral b)
MeowFloat a `meowSub` MeowFloat b   = (return . MeowFloat) (a - b)
a `meowSub` b = throwError =<< operationException "subtraction" [a, b]

meowMul :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
MeowInt a   `meowMul` MeowInt b     = (return . MeowInt) (a * b)
MeowInt a   `meowMul` MeowFloat b   = (return . MeowFloat) (fromIntegral a * b)
MeowFloat a `meowMul` MeowInt b     = (return . MeowFloat) (a * fromIntegral b)
MeowFloat a `meowMul` MeowFloat b   = (return . MeowFloat) (a * b)
a `meowMul` b = throwError =<< operationException "multiplication" [a, b]

meowDiv :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
a           `meowDiv` MeowInt 0      = throwError =<< divisionByZeroException [a, MeowInt 0]
a           `meowDiv` MeowFloat 0    = throwError =<< divisionByZeroException [a, MeowFloat 0]
MeowInt a   `meowDiv` MeowInt b      = (return . MeowInt) (a `div` b)
MeowInt a   `meowDiv` MeowFloat b    = (return . MeowFloat) (fromIntegral a / b)
MeowFloat a `meowDiv` MeowInt b      = (return . MeowFloat) (a / fromIntegral b)
MeowFloat a `meowDiv` MeowFloat b    = (return . MeowFloat) (a / b)
a `meowDiv` b = throwError =<< operationException "division" [a, b]

meowMod :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
a           `meowMod` MeowInt 0      = throwError =<< divisionByZeroException [a, MeowInt 0]
a           `meowMod` MeowFloat 0    = throwError =<< divisionByZeroException [a, MeowFloat 0]
MeowInt a   `meowMod` MeowInt b      = (return . MeowInt) (a `mod` b)
MeowInt a   `meowMod` MeowFloat b    = (return . MeowFloat) (fromIntegral a `mod'` b)
MeowFloat a `meowMod` MeowInt b      = (return . MeowFloat) (a `mod'` fromIntegral b)
MeowFloat a `meowMod` MeowFloat b    = (return . MeowFloat) (a `mod'` b)
a `meowMod` b = throwError =<< operationException "division" [a, b]

meowPow :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
MeowInt a   `meowPow` MeowInt b      = (return . MeowInt) (a ^ b)
MeowFloat a `meowPow` MeowInt b      = (return . MeowFloat) (a ^ b)
a `meowPow` b = throwError =<< operationException "power" [a, b]


{- Unary Operations -}
---------------------------------------------------------------------------------
meowNegate :: MeowPrim -> Evaluator MeowPrim
meowNegate (MeowInt a) = (return . MeowInt . negate) a
meowNegate (MeowFloat a) = (return . MeowFloat . negate) a
meowNegate a = throwError =<< operationException "negate" [a]

meowNot :: MeowPrim -> MeowPrim
meowNot = MeowBool . not . meowBool


{- Comparison Operations -}
---------------------------------------------------------------------------------
comparisonBase :: (Ordering -> Bool) -> MeowPrim -> MeowPrim -> Evaluator MeowPrim
comparisonBase f a b = MeowBool . f <$> primCompare a b

meowEq :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
meowEq = comparisonBase (== EQ)

meowLesser :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
meowLesser = comparisonBase (== LT)

meowGreater :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
meowGreater = comparisonBase (== GT)

meowLEQ :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
meowLEQ = comparisonBase (/= GT)

meowGEQ :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
meowGEQ = comparisonBase (/= LT)

meowNotEq :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
meowNotEq = comparisonBase (/= EQ)


{- String/Stack Operations -}
---------------------------------------------------------------------------------
meowPush :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
MeowString a `meowPush` MeowString b        = (return . MeowString) (a <> b)
a            `meowPush` MeowString b        = MeowString . (<> b) . toBoxedString <$> showMeow a
a            `meowPush` MeowStack xs        = (return . MeowStack) (a `boxedStackPush` xs)
a `meowPush` b = throwError =<< operationException "push" [a, b]

meowPeek :: MeowPrim -> Evaluator MeowPrim
meowPeek (MeowString xs) = let str = unboxStr xs in if Text.null str
    then return MeowNil
    else (toMeow . Text.head) str
meowPeek (MeowStack xs) = let stack = unboxStack xs in if Stack.null stack
    then return MeowNil
    else (return . Stack.peek) stack
meowPeek a = throwError =<< operationException "peek" [a]

meowPop :: MeowPrim -> Evaluator MeowPrim
meowPop (MeowString xs) = let str = unboxStr xs in if Text.null str
    then (return . MeowString) xs
    else (return . MeowString . boxedStringTail) xs
meowPop (MeowStack xs) = if (Stack.null . unboxStack) xs
    then return MeowNil
    else (return . MeowStack . boxedStackPop) xs
meowPop a = throwError =<< operationException "pop" [a]

meowConcat :: MeowPrim -> MeowPrim -> Evaluator MeowPrim
MeowStack a    `meowConcat` MeowStack b     = (return . MeowStack) (a <> b)
MeowBox a      `meowConcat` MeowBox b       = do
    let getMap = readRef . getBox
    newMap <- (<>) <$> getMap a <*> getMap b
    MeowBox . CatBox <$> newRef newMap
MeowString a   `meowConcat` MeowString b    = (return . MeowString) (a <> b)
MeowString a   `meowConcat` b               = MeowString . (a <>) . toBoxedString <$> showMeow b
a              `meowConcat` MeowString b    = MeowString . (<> b) . toBoxedString <$> showMeow a
a              `meowConcat` b               = do
    strA <- toBoxedString <$> showMeow a
    strB <- toBoxedString <$> showMeow b
    (return . MeowString) (strA <> strB)

meowLength :: MeowPrim -> Evaluator MeowPrim
meowLength (MeowString xs) = (return . MeowInt . strLen) xs
meowLength (MeowStack xs)  = (return . MeowInt . stackLen) xs
meowLength (MeowBox xs)    = MeowInt . HashMap.size <$> (readRef . getBox) xs
meowLength a = throwError =<< operationException "length" [a]
