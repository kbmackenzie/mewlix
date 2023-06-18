{-# LANGUAGE StrictData #-}

module Meowscript.Regex.Core
( MeowRegex(..)
, GroupName
, Predicate(..)
, RegexState
, Match
, Interpreter
, runInterpreter
) where

import qualified Data.Text as Text
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Except (Except, runExcept)

type GroupName = Maybe Text.Text
newtype Predicate = Predicate (Char -> Bool)

data MeowRegex =
      Verbatim Text.Text
    | AnyChar
    | Digit Bool
    | WordChar Bool
    | Whitespace Bool
    | LineStart
    | LineEnd
    | AnyListed [Predicate]
    | AnyNotListed [Predicate]
    | Count Int MeowRegex
    | CountRange (Maybe Int) (Maybe Int) MeowRegex
    | ZeroOrOne MeowRegex
    | ZeroOrMore MeowRegex
    | OneOrMore MeowRegex
    | Alternation [MeowRegex] [MeowRegex]
    | CaptureGroup GroupName [MeowRegex]
    deriving (Show)

instance Show Predicate where
    show (Predicate _) = "<predicate>"

type RegexState = Text.Text
type Match = Maybe Text.Text
type Interpreter a = StateT RegexState (Except Text.Text) a

runInterpreter :: RegexState -> Interpreter a -> Either Text.Text a
runInterpreter state interp = runExcept (fst <$> runStateT interp state)
