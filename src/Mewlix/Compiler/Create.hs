{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Create
( construct
, wrap
, asFunction
, syncCall
, asyncCall
, binaryOp
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.String.Utils ((|++), sepComma, parens)

construct :: Text -> [Text] -> Text
construct name args = Text.concat [ "new ", name, "(", sepComma args, ")" ]

wrap :: (Monad m) => Text -> m Text
wrap = return . parens

asFunction :: Text -> Text
asFunction body = Text.concat [ "(() => ", body, ")" ]

syncCall :: Text -> [Text] -> Text
syncCall name args = Text.concat [ name, "(", sepComma args, ")" ]

asyncCall :: Text -> [Text] -> Text
asyncCall name args = Text.concat [ "async ", name, "(", sepComma args, ")" ]

binaryOp :: Text -> Text -> Text -> Text
binaryOp operator a b = Text.intercalate " " [ a, operator, b ]
