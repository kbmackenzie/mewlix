{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Test.Strings
( test
) where

import Mewlix.Abstract.AST (Primitive(..))
import Mewlix.Parser (runParser, primitive)
import Test.Hspec (Spec, it, describe, shouldBe)
import Data.Text (Text)
import Mewlix.Test.Utils (generateDescription)

testStrings :: (Text, Text) -> Spec
testStrings (input, expectation) = do
    let description = generateDescription "should parse string " input
    let parse = runParser primitive "string"

    it description $
        parse input `shouldBe` Right (MewlixString expectation)

strings :: [(Text, Text)]
strings =
    [ ("'hello world'"          , "hello world"             )
    , ("\"hello world\""        , "hello world"             )
    , ("'hello \"world\"'"      , "hello \"world\""         )
    , ("\"hello 'world'\""      , "hello 'world'"           )
    , ("'hello \\'world\\''"    , "hello 'world'"           )
    , ("'hello\\nworld'"        , "hello\nworld"            )
    , ("'hello \\'world\\''"    , "hello 'world'"           )
    , ("'hello \\\"world\\\"'"  , "hello \"world\""         )
    ]

testMultilineStrings :: (Text, Text) -> Spec
testMultilineStrings (input, expectation) = do
    let description = generateDescription "should parse multiline string " input
    let parse = runParser primitive "multiline string"

    it description $
        parse input `shouldBe` Right (MewlixString expectation)

multilineStrings :: [(Text, Text)]
multilineStrings =
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
test = do
    describe "single-line strings" $
        mapM_ testStrings strings

    describe "multiline strings" $
        mapM_ testMultilineStrings multilineStrings
