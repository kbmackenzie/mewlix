{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Utils.Expression
( instantiate
, wrap
, lambda
, iife
, syncCall
, asBoolean
) where

import Data.Text (Text)
import Mewlix.String.Utils (sepComma, parens)
import qualified Mewlix.Compiler.JavaScript.Constants as Mewlix
import qualified Data.List as List

instantiate :: Text -> [Text] -> Text
instantiate name args = "new " <> name <> parens (sepComma args)

wrap :: (Monad m) => Text -> m Text
wrap = return . parens

lambda :: Text -> Text
lambda body = "(() => " <> body <> ")"

iife :: Text -> Text
iife body = lambda body <> "()"

syncCall :: Text -> [Text] -> Text
syncCall name args = name <> parens (sepComma args)

asBoolean :: Text -> Text
asBoolean = syncCall (Mewlix.conversion "toBool") . List.singleton
