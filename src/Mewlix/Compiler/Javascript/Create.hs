{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.Create
( construct
, wrap
, funcWrap
, syncCall
, asyncCall
) where

import Data.Text (Text)
import Mewlix.String.Utils (sepComma, parens)

construct :: Text -> [Text] -> Text
construct name args = "new " <> name <> parens (sepComma args)

wrap :: (Monad m) => Text -> m Text
wrap = return . parens

funcWrap :: Text -> Text
funcWrap body = "(() => " <> body <> ")"

syncCall :: Text -> [Text] -> Text
syncCall name args = name <> parens (sepComma args)

asyncCall :: Text -> [Text] -> Text
asyncCall name args = "await " <> name <> parens (sepComma args)