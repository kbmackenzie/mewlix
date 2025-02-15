{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Utils
( stringify
, boolean
, semicolon
, terminate
, findBindings
, parensAround
, lambda
, call
) where 

import Mewlix.Abstract.AST
    ( Block(..)
    , Statement(..)
    , MewlixFunction(..)
    , MewlixClass(..)
    , MewlixEnum(..)
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Mewlix.Abstract.Key (Key(..))
import Mewlix.String.Utils (sepComma, parens)
import qualified Mewlix.Compiler.JavaScript.Constants as Mewlix
import qualified Data.List as List

stringify :: Text -> Text
stringify = call Mewlix.purrify . List.singleton

boolean :: Text -> Text
boolean = call (Mewlix.convert "bool") . List.singleton

semicolon :: Text
semicolon = ";"

terminate :: Text -> Text
terminate = (<> semicolon)

findBindings :: Block -> [Key]
findBindings = do
    let collectBindings :: Statement -> [Key] -> [Key]
        collectBindings (Variable key _)    acc = key : acc
        collectBindings (Constant key _)    acc = key : acc
        collectBindings (FunctionDef func)  acc = functionName func : acc
        collectBindings (ClassDef clowder)  acc = className clowder : acc
        collectBindings (EnumDef enum)      acc = enumName enum : acc
        collectBindings _                   acc = acc

    let isPublic :: Key -> Bool
        isPublic = not . Text.isPrefixOf "_" . getKey

    filter isPublic . foldr collectBindings [] . getBlock

parensAround :: (Monad m) => Text -> m Text
parensAround = return . parens

lambda :: Text -> Text
lambda body = "(() => " <> body <> ")"

call :: Text -> [Text] -> Text
call name args = name <> parens (sepComma args)
