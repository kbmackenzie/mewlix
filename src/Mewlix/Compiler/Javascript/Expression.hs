{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.Expression
( instantiate
, wrap
, funcWrap
, syncCall
, asyncCall
) where

import Data.Text (Text)
import Mewlix.String.Utils (sepComma, parens)

instantiate :: Text -> [Text] -> Text
instantiate name args = "new " <> name <> parens (sepComma args)

wrap :: (Monad m) => Text -> m Text
wrap = return . parens

funcWrap :: Text -> Text
funcWrap body = "(() => " <> body <> ")"

syncCall :: Text -> [Text] -> Text
syncCall name args = name <> parens (sepComma args)

asyncCall :: Text -> [Text] -> Text
asyncCall name args = "await " <> name <> parens (sepComma args)
