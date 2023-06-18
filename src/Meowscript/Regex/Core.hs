{-# LANGUAGE StrictData #-}

module Meowscript.Regex.Core
( MeowRegex(..)
, GroupName
, Predicate(..)
) where

import qualified Data.Text as Text

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
