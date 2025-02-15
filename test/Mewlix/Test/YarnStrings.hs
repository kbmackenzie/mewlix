{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Test.YarnStrings
( test
) where

import Mewlix.Abstract.AST
    ( Expression(..)
    , Primitive(..)
    , BinaryOp(..)
    )
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Parser (runParser, expression)
import Mewlix.Test.Utils (generateDescription)
import Data.Text (Text)
import Test.Hspec (Spec, it, shouldBe)
import Prelude hiding (lookup, negate, concat, and, or)

testYarnString :: [(Text, Expression)] -> Spec
testYarnString = do
    let parseExpr :: (Text, Expression) -> Spec
        parseExpr (input, expected) = do
            let description = generateDescription "should parse yarn string " input
            let parse = runParser expression "yarn string"

            it description $
                parse input `shouldBe` Right expected
    mapM_ parseExpr

yarnStrings :: [(Text, Expression)]
yarnStrings = 
    [ (":3'2 + 2 = [2 + 2]'"        , concat (str "2 + 2 = ") (add (number 2) (number 2))    )
    , (":3'[2 + 2] is 2 + 2'"       , concat (add (number 2) (number 2)) (str " is 2 + 2")   )
    , (":3\"[2 + 2] is 2 + 2\""     , concat (add (number 2) (number 2)) (str " is 2 + 2")   )
    , (":3'hello ['world']'"        , concat (str "hello ") (str "world")                    )
    , (":3'hello [\"world\"]'"      , concat (str "hello ") (str "world")                    )
    , (":3\"hello ['world']\""      , concat (str "hello ") (str "world")                    )
    , (":3\"hello [\"world\"]\""    , concat (str "hello ") (str "world")                    )
    , (":3'hello ['wor' .. 'ld']'"  , concat (str "hello ") (concat (str "wor") (str "ld"))  )
    , (":3'hello [a['b']]'"         , concat (str "hello ") (lookup (variable "a") (str "b")))
    , (":3'hello [[]]'"             , concat (str "hello ") (shelf mempty)                   )
    , (":3'hello [['world']]'"      , concat (str "hello ") (shelf [str "world"])            )
    , (":3''"                       , str ""                                                 )
    , (":3'[2 + 2]'"                , coerce (add (number 2) (number 2))                     )
    , (":3'[:3'']'"                 , coerce (str "")                                        )
    ]
    where str      = PrimitiveExpr . MewlixString
          concat   = BinaryOperation StringConcat
          number   = PrimitiveExpr . MewlixInt
          add      = BinaryOperation Addition
          shelf    = ShelfExpression
          variable = Identifier . Key
          lookup   = (. StringCoerce) . LookupExpression
          coerce   = StringCoerce

test :: Spec
test = testYarnString yarnStrings
