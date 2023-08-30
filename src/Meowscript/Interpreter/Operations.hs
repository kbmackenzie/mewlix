{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Interpreter.Operations
( meowAdd
, meowSub
, meowMul
, meowDiv
, meowMod
, meowPow
, meowPush
, meowPeek
, meowPop
, meowConcat
, meowLength
) where

import Meowscript.Abstract.Atom
import Meowscript.Abstract.Meowable
import Meowscript.Abstract.Primitive
import Meowscript.Evaluate.Evaluator
import Meowscript.Interpreter.Exceptions
import qualified Data.Text as Text
import qualified Meowscript.Data.Stack as Stack
import qualified Data.HashMap.Strict as HashMap
import Data.Fixed (mod')

{- Arithmetic Operations -}
---------------------------------------------------------------------------------
meowAdd :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m MeowAtom
MeowInt a   `meowAdd` MeowInt b     = (return . MeowInt) (a + b)
MeowInt a   `meowAdd` MeowFloat b   = (return . MeowFloat) (fromIntegral a + b)
MeowFloat a `meowAdd` MeowInt b     = (return . MeowFloat) (a + fromIntegral b)
MeowFloat a `meowAdd` MeowFloat b   = (return . MeowFloat) (a + b)
a `meowAdd` b = throwException =<< operationException "addition" [a, b]

meowSub :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m MeowAtom
MeowInt a   `meowSub` MeowInt b     = (return . MeowInt) (a - b)
MeowInt a   `meowSub` MeowFloat b   = (return . MeowFloat) (fromIntegral a - b)
MeowFloat a `meowSub` MeowInt b     = (return . MeowFloat) (a - fromIntegral b)
MeowFloat a `meowSub` MeowFloat b   = (return . MeowFloat) (a - b)
a `meowSub` b = throwException =<< operationException "subtraction" [a, b]

meowMul :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m MeowAtom
MeowInt a   `meowMul` MeowInt b     = (return . MeowInt) (a * b)
MeowInt a   `meowMul` MeowFloat b   = (return . MeowFloat) (fromIntegral a * b)
MeowFloat a `meowMul` MeowInt b     = (return . MeowFloat) (a * fromIntegral b)
MeowFloat a `meowMul` MeowFloat b   = (return . MeowFloat) (a * b)
a `meowMul` b = throwException =<< operationException "multiplication" [a, b]

meowDiv :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m MeowAtom
a           `meowDiv` MeowInt 0      = throwException =<< divisionByZeroException [a, MeowInt 0]
a           `meowDiv` MeowFloat 0    = throwException =<< divisionByZeroException [a, MeowFloat 0]
MeowInt a   `meowDiv` MeowInt b      = (return . MeowInt) (a `div` b)
MeowInt a   `meowDiv` MeowFloat b    = (return . MeowFloat) (fromIntegral a / b)
MeowFloat a `meowDiv` MeowInt b      = (return . MeowFloat) (a / fromIntegral b)
MeowFloat a `meowDiv` MeowFloat b    = (return . MeowFloat) (a / b)
a `meowDiv` b = throwException =<< operationException "division" [a, b]

meowMod :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m MeowAtom
a           `meowMod` MeowInt 0      = throwException =<< divisionByZeroException [a, MeowInt 0]
a           `meowMod` MeowFloat 0    = throwException =<< divisionByZeroException [a, MeowFloat 0]
MeowInt a   `meowMod` MeowInt b      = (return . MeowInt) (a `mod` b)
MeowInt a   `meowMod` MeowFloat b    = (return . MeowFloat) (fromIntegral a `mod'` b)
MeowFloat a `meowMod` MeowInt b      = (return . MeowFloat) (a `mod'` fromIntegral b)
MeowFloat a `meowMod` MeowFloat b    = (return . MeowFloat) (a `mod'` b)
a `meowMod` b = throwException =<< operationException "division" [a, b]

meowPow :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m MeowAtom
MeowInt a   `meowPow` MeowInt b      = (return . MeowInt) (a ^ b)
MeowFloat a `meowPow` MeowInt b      = (return . MeowFloat) (a ^ b)
a `meowPow` b = throwException =<< operationException "power" [a, b]


{- String/Stack Operations -}
---------------------------------------------------------------------------------
meowPush :: (MonadIO m, MeowThrower m) => MeowAtom -> MeowAtom -> m MeowAtom
MeowString a `meowPush` MeowString b        = (return . MeowString) (a <> b)
a            `meowPush` MeowString b        = MeowString . (<> b) . boxString <$> showMeow a
a            `meowPush` MeowStack xs         = (return . MeowStack) (a `stackPush` xs)
a `meowPush` b = throwException =<< operationException "push" [a, b]

meowPeek :: (MonadIO m, MeowThrower m) => MeowAtom -> m MeowAtom
meowPeek (MeowString xs) = let str = unboxStr xs in if Text.null str
    then return MeowNil
    else (toMeow . Text.head) str
meowPeek (MeowStack xs) = let stack = unboxStack xs in if Stack.null stack
    then return MeowNil
    else (return . Stack.peek) stack
meowPeek a = throwException =<< operationException "peek" [a]

meowPop :: (MonadIO m, MeowThrower m) => MeowAtom -> m MeowAtom
meowPop (MeowString xs) = let str = unboxStr xs in if Text.null str
    then (return . MeowString) xs
    else (return . MeowString . strTail) xs
meowPop (MeowStack xs) = if (Stack.null . unboxStack) xs
    then return MeowNil
    else (return . MeowStack . stackPop) xs
meowPop a = throwException =<< operationException "pop" [a]

meowConcat :: (MonadIO m) => MeowAtom -> MeowAtom -> m MeowAtom
MeowStack a    `meowConcat` MeowStack b     = (return . MeowStack) (a <> b)
MeowBox a      `meowConcat` MeowBox b       = (return . MeowBox) (a <> b)
MeowString a   `meowConcat` MeowString b    = (return . MeowString) (a <> b)
MeowString a   `meowConcat` b               = MeowString . (a <>) . boxString <$> showMeow b
a              `meowConcat` MeowString b    = MeowString . (<> b) . boxString <$> showMeow a
a              `meowConcat` b               = do
    strA <- boxString <$> showMeow a
    strB <- boxString <$> showMeow b
    (return . MeowString) (strA <> strB)

meowLength :: (MonadIO m, MeowThrower m) => MeowAtom -> m MeowAtom
meowLength (MeowString xs) = (return . MeowInt . fromIntegral . strLen) xs
meowLength (MeowStack xs)  = (return . MeowInt . fromIntegral . stackLen) xs
meowLength (MeowBox xs)    = (return . MeowInt . fromIntegral . HashMap.size) xs
meowLength a = throwException =<< operationException "length" [a]
