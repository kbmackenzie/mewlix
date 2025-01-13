{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Test.Strings
( test
) where

import Mewlix.Abstract.AST (Primitive(..))
import Mewlix.Parser (runParser, primitive)
import Test.Hspec (Spec, it, shouldBe)
import Data.Text (Text)

multiline :: (Text, Text) -> Spec
multiline (input, expectation) = let parse = runParser primitive "multiline string" in
    it "should parse multiline string" $
        parse input `shouldBe` Right (MewlixString expectation)

testInput :: [(Text, Text)]
testInput =
    [ ("'''\nhello\nworld\n'''"     , "hello\nworld\n"      )
    , ("\"\"\"hello\nworld\n\"\"\"" , "hello\nworld\n"      )
    , ("'''hello\nworld\n'''"       , "hello\nworld\n"      )
    , ("''''''"                     , ""                    )
    , ("\"\"\"\"\"\""               , ""                    )
    , ("'''\n'''"                   , ""                    )
    , ("\"\"\"\n\"\"\""             , ""                    )
    , ("'''\n\n'''"                 , "\n"                  )
    , ("\"\"\"\n\n\"\"\""           , "\n"                  )
    ]

test :: Spec
test = mapM_ multiline testInput
