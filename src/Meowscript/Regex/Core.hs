{-# LANGUAGE StrictData #-}

module Meowscript.Regex.Core
( RegexAST(..)
, GroupName
, Predicate(..)
, Input
, RegexOutput(..)
, StateMachine
, RegexState
, runStateMachine
, joinPredicates
) where

import qualified Data.Text as Text
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad (liftM2)

type GroupName = Maybe Text.Text
newtype Predicate = Predicate { getPredicate :: Char -> Bool }

data RegexAST =
      Verbatim Text.Text
    | AnyChar
    | LineStart
    | LineEnd
    | CharacterClass Predicate
    | Count Int RegexAST
    | CountRange (Maybe Int) (Maybe Int) RegexAST
    | ZeroOrOne RegexAST
    | ZeroOrMore RegexAST
    | OneOrMore RegexAST
    | Alternation [RegexAST] [RegexAST]
    | CaptureGroup GroupName [RegexAST]
    deriving (Show)

instance Show Predicate where
    show (Predicate _) = "<predicate>"

type Input = Text.Text
data RegexOutput = RegexOutput Text.Text [Text.Text]

type RegexState = Text.Text
type StateMachine a = MaybeT (StateT RegexState Identity) a

runStateMachine :: RegexState -> StateMachine a -> (Maybe a, RegexState)
runStateMachine state interp = runIdentity (runStateT (runMaybeT interp) state)

joinPredicates :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
joinPredicates = liftM2 (||)
