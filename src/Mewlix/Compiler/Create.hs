{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Create
( construct
, wrap
, asFunction
, syncCall
, asyncCall
, assignment
, lambdaFunc
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.String.Utils ((|++), sepComma, parens)
import qualified Mewlix.Compiler.Constants as Mewlix
import Mewlix.Abstract.AST (BinaryOp(..), UnaryOp(..))

construct :: Text -> [Text] -> Text
construct name args = Text.concat [ "new ", name, "(", sepComma args, ")" ]

wrap :: (Monad m) => Text -> m Text
wrap = return . parens

asFunction :: Text -> Text
asFunction body = Text.concat [ "(() => ", body, ")" ]

syncCall :: Text -> [Text] -> Text
syncCall name args = Text.concat [ name, "(", sepComma args, ")" ]

asyncCall :: Text -> [Text] -> Text
asyncCall name args = Text.concat [ "await ", name, "(", sepComma args, ")" ]

assignment :: Text -> Text -> Text
assignment a b = Text.concat [ a, " = ", b ]

lambdaFunc :: [Text] -> Text -> Text
lambdaFunc params body = Text.concat [ "(", sepComma params, ") => ", body ]
