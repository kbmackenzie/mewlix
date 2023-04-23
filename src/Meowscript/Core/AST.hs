--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Meowscript.Core.AST
( Prim(..)
, Expr(..)
, Unop(..)
, Binop(..)
, Evaluate(..)
, Statement(..)
, Environment
, Args
) where

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Control.Monad.Reader as Reader
--import qualified Text.Megaparsec as Mega
--import qualified Text.Megaparsec.Char as MChar
--import qualified Text.Megaparsec.Char.Lexer as Lexer

type Args = [Text.Text]

data Prim =
      MeowString Text.Text
    | MeowAtom Text.Text
    | MeowBool Bool
    | MeowInt Int
    | MeowNumber Double
    | MeowLonely
    | MeowFunc Text.Text Args [Statement]
    deriving (Eq, Show, Ord)

data Expr =
      EPrim Prim
    | EUnop Unop Expr 
    | EBinop Binop Expr Expr
    | EWhitespace
    deriving (Eq, Show, Ord)

data Statement =
      SExpr Expr
    | SWhile Expr [Statement]
    | SIfElse Expr [Statement] [Statement] 
    | SFuncDef Text.Text Args [Statement]
    deriving (Eq, Show, Ord)

data Unop =
      MeowNot 
    | MeowYarn 
    | MeowYarnLen 
    | MeowCall 
    | MeowNegate
    deriving (Eq, Show, Ord)

data Binop =
      MeowAdd
    | MeowSub 
    | MeowMul 
    | MeowDiv 
    | MeowAnd 
    | MeowOr 
    | MeowCompare [Ordering]
    | MeowAssign 
    | MeowPoke 
    | MeowConcat 
    deriving (Eq, Show, Ord)


{- Variable context and evaluation monad. -}

type Environment = Map.Map Text.Text Prim

newtype Evaluate a = Evaluate { unEval :: Reader.ReaderT Environment IO a }
  deriving ( Monad
           , Functor
           , Applicative
           , Reader.MonadReader Environment
           , Reader.MonadIO)
