{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Interpreter.Boxes
( getBox
, boxPeek
, boxWrite
, asIdentifier
) where

import Meowscript.Abstract.Atom
import Meowscript.Data.Ref
import Meowscript.Evaluate.Evaluator
import Meowscript.Interpreter.Exceptions
import Meowscript.Parser.AST
import qualified Data.HashMap.Strict as HashMap

type Box = HashMap.HashMap Identifier AtomRef

getBox :: (MonadIO m, MeowThrower m) => MeowAtom -> m Box
getBox atom = case atom of
    (MeowBox box) -> return box
    _             -> throwException =<< notABoxException [atom]

boxPeek :: (MonadIO m, MeowThrower m) => Identifier -> MeowAtom -> m (Ref MeowAtom)
boxPeek key atom = getBox atom >>= \box -> case HashMap.lookup key box of
    Nothing       -> throwException =<< notAPropertyException key [atom]
    (Just ref)    -> return ref

boxWrite :: (MonadIO m, MeowThrower m) => Identifier -> MeowAtom -> Ref MeowAtom -> m ()
boxWrite key value boxRef = do
    box <- readRef boxRef >>= getBox
    case HashMap.lookup key box of
        Nothing       -> do
            ref <- newRef value
            let newBox = MeowBox (HashMap.insert key ref box)
            writeRef newBox boxRef
        (Just ref)    -> writeRef value ref

asIdentifier :: (MonadIO m, MeowThrower m) => MeowAtom -> m Identifier
asIdentifier value = case value of
    (MeowString str) -> (return . unboxStr) str
    _                -> throwException =<< notAnIdentifier [value]
