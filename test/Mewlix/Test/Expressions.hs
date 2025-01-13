{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Test.Expressions
( test
) where

import Mewlix.Abstract.AST
    ( Expression(..)
    , Primitive(..)
    , BinaryOp(..)
    , UnaryOp(..)
    , Params(..)
    , Arguments(..)
    )
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Parser (runParser, expression)
import Mewlix.Test.Utils (generateDescription)
import Data.Text (Text)
import Test.Hspec (Spec, it, shouldBe)
import Prelude hiding (lookup, negate, concat, and, or)
import qualified Data.List as List

testExpressions :: [(Text, Expression)] -> Spec
testExpressions = do
    let parseExpr :: (Text, Expression) -> Spec
        parseExpr (input, expected) = do
            let description = generateDescription "should parse the expression " input
            let parse = runParser expression "expression"

            it description $
                parse input `shouldBe` Right expected
    mapM_ parseExpr

expressions :: [(Text, Expression)]
expressions = 
    -- Basic expressions:
    [ ("1 + 2"              , add (number 1) (number 2)                                                 )
    , ("-1 + 2"             , add (negate (number 1)) (number 2)                                        )
    , ("1 + 2 * 3"          , add (number 1) (multiply (number 2) (number 3))                           )
    , ("(1 + 2) * 3"        , multiply (add (number 1) (number 2)) (number 3)                           )
    , ("a .. b .. c"        , concat (concat (variable "a") (variable "b")) (variable "c")              )
    , ("a..b..c"            , concat (concat (variable "a") (variable "b")) (variable "c")              )
    , ("a push b"           , push (variable "a") (variable "b")                                        )
    , ("a push b push c"    , push (push (variable "a") (variable "b")) (variable "c")                  )
    , ("paw at a push b"    , push (peek (variable "a")) (variable "b")                                 )
    , ("a and b or c"       , or (and (variable "a") (variable "b")) (variable "c")                     )
    , ("a or b and c"       , or (variable "a") (and (variable "b") (variable "c"))                     )
    , ("a if b else c"      , ternary (variable "b") (variable "a") (variable "c")                      )
    , ("true or false"      , or true false                                                             )
    , ("a if b or c else d" , ternary (or (variable "b") (variable "c")) (variable "a") (variable "d")  )
    , ("a == true"          , equal (variable "a") true                                                 )
    , ("true == a"          , equal true (variable "a")                                                 )
    , ("a == b == c"        , equal (equal (variable "a") (variable "b")) (variable "c")                )
    , ("a .. b == c"        , equal (concat (variable "a") (variable "b")) (variable "c")               )
    , ("a == b .. c"        , equal (variable "a") (concat (variable "b") (variable"c"))                )
    -- Functions calls + property access:
    , ("a()()()"            , call' (call' (call' (variable "a")))                                      )
    , ("a()['b']()"         , call' (lookup (call' (variable "a")) (str "b"))                           )
    , ("a['b']()()"         , call' (call' (lookup (variable "a") (str "b")))                           )
    , ("a()['b']['c']()()"  , call' (call' (lookup (lookup (call' (variable "a")) (str "b")) (str "c"))))
    , ("a['b']()['c']()()"  , call' (call' (lookup (call' (lookup (variable "a") (str "b"))) (str "c"))))
    , ("a.b()['c']"         , lookup (call' (dot (variable "a") (prop "b"))) (str "c")                  )
    , ("a['b'].c()"         , call' (dot (lookup (variable "a") (str "b")) (prop "c"))                  )
    , ("a.b.c.d"            , dot (dot (dot (variable "a") (prop "b")) (prop "c")) (prop "d")           )
    , ("a().b.c.d"          , dot (dot (dot (call' (variable "a")) (prop "b")) (prop "c")) (prop "d")   )
    -- Lambdas:
    , ("=^oxo^= (x) -> x"   , lambda (param "x") (variable "x")                                         )
    , ("🐈 (x) -> x"        , lambda (param "x") (variable "x")                                         )
    , ("(🐈 (x) -> x)()"    , call' (lambda (param "x") (variable "x"))                                 )
    , ("(🐈 (x) -> x)(3)"   , call (lambda (param "x") (variable "x")) (args [number 3])                )
    , ("🐈 () -> '🐈'"      , lambda mempty (str "🐈")                                                  )
    -- Function calls:
    , ("a(b)"               , call (variable "a") (args [variable "b"])                                 )
    , ("a(b, c, d)"         , call (variable "a") (args [variable "b", variable "c", variable "d"])     )
    , ("a(b)(c)"            , call (call (variable "a") (args [variable "b"])) (args [variable "c"])    )
    , ("a(\nb,\nc,\n)"      , call (variable "a") (args [variable "b", variable "c"])                   )
    , ("a(\nb,\nc,\n)"      , call (variable "a") (args [variable "b", variable "c"])                   )
    -- Shelves:
    , ("[]"                 , shelf []                                                                  )
    , ("[\n]"               , shelf []                                                                  )
    , ("[\n\n\n]"           , shelf []                                                                  )
    , ("[a, b, c]"          , shelf [variable "a", variable "b", variable "c"]                          ) 
    , ("[\na,\nb,\nc,\n]"   , shelf [variable "a", variable "b", variable "c"]                          )
    , ("[\na,\nb,\nc,\n]"   , shelf [variable "a", variable "b", variable "c"]                          )
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
          ternary  = TernaryOperation
          true     = PrimitiveExpr . MewlixBool $ True
          false    = PrimitiveExpr . MewlixBool $ False
          equal    = BinaryOperation Equal
          lookup   = LookupExpression
          dot      = DotExpression
          str      = PrimitiveExpr . MewlixString
          prop     = ObjectProperty . Key
          lambda   = LambdaExpression
          param    = Params . List.singleton . Key
          args     = Arguments
          call     = FunctionCall
          call'    = flip FunctionCall mempty
          shelf    = ShelfExpression

test :: Spec
test = testExpressions expressions
