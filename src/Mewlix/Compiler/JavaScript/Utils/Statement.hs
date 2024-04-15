{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.JavaScript.Utils.Statement
( terminate
, findBindings
) where

import Mewlix.Abstract.AST
    ( Block(..)
    , Statement(..)
    , MewlixFunction(..)
    , MewlixClass(..)
    )
import Data.Text (Text)
import Mewlix.Abstract.Key (Key(..))

semicolon :: Text
semicolon = ";"

terminate :: Text -> Text
terminate = (<> semicolon)

findBindings :: Block -> [Key]
findBindings block = do
    let collectBindings :: Statement -> [Key] -> [Key]
        collectBindings (Variable key _)    acc = key : acc
        collectBindings (Constant key _)    acc = key : acc
        collectBindings (FunctionDef func)  acc = funcName func : acc
        collectBindings (ClassDef clowder)  acc = className clowder : acc
        collectBindings _                   acc = acc
    foldr collectBindings [] (getBlock block)
