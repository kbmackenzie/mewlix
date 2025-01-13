{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Test.Expressions
( test
) where

import Mewlix.Abstract.AST
    ( Expression(..)
    , Primitive(..)
    , BinaryOp(..)
    , UnaryOp(..)
    )
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Parser (runParser, expression)
import Mewlix.Test.Utils (generateDescription)
import Data.Text (Text)
import Test.Hspec
    ( Spec
    , it
    , describe
    , shouldBe
    )
import Prelude hiding (negate, concat, and, or)

expressions :: [(Text, Expression)] -> Spec
expressions = describe "parse mewlix expressions" . do
    let parseExpr :: (Text, Expression) -> Spec
        parseExpr (input, expected) = do
            let description = generateDescription "should parse the expression " input
            let parse = runParser expression "expression"

            it description $
                parse input `shouldBe` Right expected
    mapM_ parseExpr

basicExpressions :: [(Text, Expression)]
basicExpressions = 
    [ ("1 + 2"              , add (number 1) (number 2)                                    )
    , ("-1 + 2"             , add (negate (number 1)) (number 2)                           )
    , ("1 + 2 * 3"          , add (number 1) (multiply (number 2) (number 3))              )

    , ("(1 + 2) * 3"        , multiply (add (number 1) (number 2)) (number 3)              )
    , ("a .. b .. c"        , concat (concat (variable "a") (variable "b")) (variable "c") )
    , ("a..b..c"            , concat (concat (variable "a") (variable "b")) (variable "c") )
    , ("a push b"           , push (variable "a") (variable "b")                           )
    , ("a push b push c"    , push (push (variable "a") (variable "b")) (variable "c")     )
    , ("paw at a push b"    , push (peek (variable "a")) (variable "b")                    )
    , ("a and b or c"       , or (and (variable "a") (variable "b")) (variable "c")        )
    , ("a or b and c"       , or (variable "a") (and (variable "b") (variable "c"))        )
    ]
    where number   = PrimitiveExpr . MewlixInt
          add      = BinaryOperation Addition
          multiply = BinaryOperation Multiplication
          negate   = UnaryOperation Minus
          variable = Identifier . Key
          concat   = BinaryOperation StringConcat
          push     = BinaryOperation ShelfPush
          peek     = UnaryOperation ShelfPeek
          and      = BooleanAnd
          or       = BooleanOr

test :: Spec
test = expressions basicExpressions
