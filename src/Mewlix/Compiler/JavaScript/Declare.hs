{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Declare
( declareOperations
, operation
) where

import Mewlix.Compiler.Analysis (Operation(..))
import Mewlix.Keywords.Shadow (shadow)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

operation :: Operation -> Text
operation op = case op of
    Or      -> shadow "or"
    And     -> shadow "and"
    Ternary -> shadow "ternary"

declareOperations :: Set Operation -> Maybe Text
declareOperations operations = do
    if Set.null operations
        then Nothing
        else Just $ do
            let list = map operation (Set.toList operations)
            Text.concat [ "let ", Text.intercalate ", " list, ";" ]
