{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Test.Assignment
( test
) where

import Mewlix.Abstract.AST
    ( Expression(..)
    , Primitive(..)
    , Statement(..)
    , YarnBall(..)
    , Block(..)
    , BinaryOp(..)
    )
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Parser (runParser, root)
import Mewlix.Test.Utils (generateDescription)
import Data.Text (Text)
import Test.Hspec (Spec, it, shouldBe)
import Prelude hiding (lookup, negate, concat, and, or)

testAssignment :: [(Text, Statement)] -> Spec
testAssignment = do
    let parseStatement :: (Text, Statement) -> Spec
        parseStatement (input, expected) = do
            let description = generateDescription "should parse assignment " input
            let parse = fmap (getBlock . yarnballBlock) . runParser root "assignment"

            it description $
                parse input `shouldBe` Right [expected]
    mapM_ parseStatement

assignments :: [(Text, Statement)]
assignments = 
    [ ("mew a = 2"          , var (Key "a") (number 2)                                              )
    , ("mew a = 3 + 4"      , var (Key "a") (add (number 3) (number 4))                             )
    , ("mew a = b == c"     , var (Key "a") (equal (variable "b") (variable "c"))                   )
    , ("mew a = b.c.d"      , var (Key "a") (dot (dot (variable "b") (prop "c")) (prop "d"))        )
    , ("mew a = b['c']"     , var (Key "a") (lookup (variable "b") (str "c"))                       )
    , ("mew a = b['c']()"   , var (Key "a") (call (lookup (variable "b") (str "c")))                )
    , ("mew a!!! = 3 + 4"   , constant (Key "a") (add (number 3) (number 4))                        )
    , ("mew a!!!!!!!"       , constant (Key "a") nil                                                )
    , ("mew a! = b == c"    , constant (Key "a") (equal (variable "b") (variable "c"))              )
    , ("mew a!!! = b.c.d"   , constant (Key "a") (dot (dot (variable "b") (prop "c")) (prop "d"))   )
    , ("mew a!!! = b['c']"  , constant (Key "a") (lookup (variable "b") (str "c"))                  )
    , ("mew a! = b['c']()"  , constant (Key "a") (call (lookup (variable "b") (str "c")))           )
    , ("mew a"              , var (Key "a") nil                                                     )
    , ("a = b"              , assign (variable "a") (variable "b")                                  )
    , ("a = b == c"         , assign (variable "a") (equal (variable "b") (variable "c"))           )
    ]
    where assign   = Assignment
          var      = Variable
          constant = Constant
          nil      = PrimitiveExpr MewlixNil
          number   = PrimitiveExpr . MewlixInt
          add      = BinaryOperation Addition
          variable = Identifier . Key
          equal    = BinaryOperation Equal
          call     = flip FunctionCall mempty
          lookup   = (. StringCoerce) . LookupExpression
          dot      = DotExpression
          prop     = ObjectProperty . Key
          str      = PrimitiveExpr . MewlixString

test :: Spec
test = testAssignment assignments
