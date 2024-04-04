{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Utils.Expression
( instantiate
, wrap
, lambda
, asyncLambda
, iife
, syncCall
, asyncCall
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

asyncLambda :: Text -> Text
asyncLambda body = "(async () => " <> body <> ")"

iife :: Text -> Text
iife body = lambda body <> "()"

syncCall :: Text -> [Text] -> Text
syncCall name args = name <> parens (sepComma args)

asyncCall :: Text -> [Text] -> Text
asyncCall name args = "await " <> name <> parens (sepComma args)

asBoolean :: Text -> Text
asBoolean = syncCall (Mewlix.conversion "toBool") . List.singleton
