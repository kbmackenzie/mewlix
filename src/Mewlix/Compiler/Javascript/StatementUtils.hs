{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Javascript.StatementUtils
( terminate
, findImports
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
import Mewlix.Abstract.Module (ModuleData(..))

semicolon :: Text
semicolon = ";"

terminate :: Text -> Text
terminate = (<> semicolon)

findImports :: Block -> [ModuleData]
findImports block = do
    let collectImports :: Statement -> [ModuleData] -> [ModuleData]
        collectImports (ImportStatement dat) acc = dat : acc
        collectImports _                     acc = acc
    foldr collectImports [] (getBlock block)

findBindings :: Block -> [Key]
findBindings block = do
    let collectBindings :: Statement -> [Key] -> [Key]
        collectBindings (Binding key _)     acc = key : acc
        collectBindings (FunctionDef func)  acc = funcName func : acc
        collectBindings (ClassDef clowder)  acc = className clowder : acc
        collectBindings _                   acc = acc
    foldr collectBindings [] (getBlock block)
