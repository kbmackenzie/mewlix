{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Test.Strings
( test
) where

import Mewlix.Abstract.AST (Primitive(..))
import Mewlix.Parser (runParser, primitive)
import Test.Hspec (Spec, it, shouldBe)
import Data.Text (Text)
import Mewlix.Test.Utils (generateDescription)

multiline :: (Text, Text) -> Spec
multiline (input, expectation) = do
    let description = generateDescription "should parse multiline string " input
    let parse = runParser primitive "multiline string"

    it description $
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
