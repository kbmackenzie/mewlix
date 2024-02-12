{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.ExpressionUtils
( instantiate
, wrap
, funcWrap
, iifeWrap
, syncCall
, asyncCall
, asBoolean
) where

import Data.Text (Text)
import Mewlix.String.Utils (sepComma, parens)
import qualified Mewlix.Compiler.Javascript.Constants as Mewlix
import qualified Data.List as List

instantiate :: Text -> [Text] -> Text
instantiate name args = "new " <> name <> parens (sepComma args)

wrap :: (Monad m) => Text -> m Text
wrap = return . parens

funcWrap :: Text -> Text
funcWrap body = "(() => " <> body <> ")"

iifeWrap :: Text -> Text
iifeWrap body = funcWrap body <> "()"

syncCall :: Text -> [Text] -> Text
syncCall name args = name <> parens (sepComma args)

asyncCall :: Text -> [Text] -> Text
asyncCall name args = "await " <> name <> parens (sepComma args)

asBoolean :: Text -> Text
asBoolean = syncCall (Mewlix.operation "toBool") . List.singleton
