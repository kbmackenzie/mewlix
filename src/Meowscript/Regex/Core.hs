{-# LANGUAGE StrictData #-}

module Meowscript.Regex.Core
( MeowRegex(..)
, GroupName
, Predicate(..)
, Matcher
, RegexState
, runMatcher
) where

import qualified Data.Text as Text
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Identity (Identity, runIdentity)

type GroupName = Maybe Text.Text
newtype Predicate = Predicate { getPredicate :: Char -> Bool }

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
type Matcher a = MaybeT (StateT RegexState Identity) a

runMatcher :: RegexState -> Matcher a -> (Maybe a, Text.Text)
runMatcher state interp = runIdentity (runStateT (runMaybeT interp) state)
