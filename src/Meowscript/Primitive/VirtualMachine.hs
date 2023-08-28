{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Bytecode.VirtualMachine
(
) where

import Meowscript.Bytecode.Prim
import Meowscript.Bytecode.OpCode
import Meowscript.Bytecode.Evaluator
import Meowscript.Bytecode.Operations
import Meowscript.Bytecode.PrimOperations
import Meowscript.Bytecode.Meowable
import qualified Data.Vector as Vector
import Lens.Micro.Platform (set, over, view)
import Data.Functor ((<&>))
import Control.Monad (void, replicateM, replicateM_)

data ReturnValue =
      ReturnVoid
    | ReturnJump StackIndex
    | ReturnPrim MeowPrim

type MeowVM a = Chunk MeowPrim -> MeowMachine a

getNext :: MeowVM ReturnValue
{-# INLINE getNext #-}
getNext chunk = getIp >>= \ip -> if ip <= chunkSize chunk
    then return ReturnVoid
    else incrIp >> runOp (chunkVector chunk Vector.! succ ip) >>= \case
        (ReturnPrim p) -> return (ReturnPrim p)
        _ -> getNext chunk


runOp :: MeowOp MeowPrim -> MeowMachine ReturnValue
{-# INLINE runOp #-}
runOp Nop = return ReturnVoid

-- Stack shuffle:
runOp Pop = popAny >> return ReturnVoid
runOp (Push c) = push (Constant c) >> return ReturnVoid
runOp (PopMany n) = replicateM_ n popAny >> return ReturnVoid

-- Jumps:
runOp (JumpBy a) = modifyIp (const a) >> return ReturnVoid
runOp (RewindBy a) = modifyIp (const a) >> return ReturnVoid
runOp (JumpIfFalse a) = pop >>= \j -> if meowBool j
    then return ReturnVoid
    else modifyIp (const a) >> return (ReturnJump a)
runOp (ConditionalJump a b) = pop >>= \j -> if meowBool j
    then modifyIp (const a) >> return ReturnVoid
    else modifyIp (const b) >> return ReturnVoid

{- Object Creation -}
runOp (MakeObject keys) = do
    pairs <- zip keys <$> replicateM (length keys) pop
    (liftIO . toMeow . MeowPairs) pairs >> return ReturnVoid

{- Arithmetic Operations -}
runOp Addition       = vmOp opAdd >>= pushConst
runOp Subtraction    = vmOp opSub >>= pushConst
runOp Multiplication = vmOp opMul >>= pushConst
runOp Division       = vmOp opDiv >>= pushConst
runOp Modulo         = vmOp opMod >>= pushConst
runOp Power          = vmOp opPow >>= pushConst

{- Unary Operations -}
runOp Negation       = pop >>= opNegate >>= pushConst
runOp BooleanNot     = pop >>= opNot >>= pushConst

{- Boolean Operations -}
runOp (BooleanAnd i) = peek >>= \value -> if meowBool value
    then pop >> return ReturnVoid
    else modifyIp (const i) >> return ReturnVoid

runOp (BooleanOr i)  = peek >>= \value -> if meowBool value
    then modifyIp (const i) >> return ReturnVoid
    else pop >> return ReturnVoid

{- Local Variables -}
runOp (DefLocal key) = peek >>= opDefLocal key >> return ReturnVoid
runOp (SetLocal adr) = peek >>= opSetLocal adr >> return ReturnVoid
runOp (GetLocal adr) = opGetLocal adr >>= push >> return ReturnVoid

{- Global Variables -}
runOp (DefGlobal key) = peek >>= globalDef key >> return ReturnVoid
runOp (SetGlobal key) = peek >>= globalSet key >> return ReturnVoid
runOp (GetGlobal key) = globalGet key >>= \case
    (Just value) -> pushConst value
    Nothing -> throwE undefined -- todo

runOp _ = undefined

{- VM Operation Base -}
vmOp :: (MeowPrim -> MeowPrim -> MeowMachine MeowPrim) -> MeowMachine MeowPrim
vmOp f = (,) <$> pop <*> pop >>= uncurry f

pushConst :: MeowPrim -> MeowMachine ReturnValue
pushConst = (>> return ReturnVoid) . push . Constant

{-data MeowOp p =
      Nop
    | Pop
    | Push p
    | JumpBy IpIncrement
    | JumpIfFalse IpIncrement
    | ConditionalJump IpIncrement IpIncrement
    | GetGlobal Key
    | SetGlobal Key
    | DefGlobal Key
    | DefLocal StackIndex
    | GetLocal StackIndex
    | SetLocal StackIndex
    | GetUpValue StackIndex
    | SetUpValue StackIndex
    | Addition
    | Subtraction
    | Multiplication
    | Division
    | Modulo
    | Power
    | Negation
    | Concat
    | ListPeek
    | ListPush
    | ListPop
    deriving (Show)

 -}
