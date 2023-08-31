{-# LANGUAGE OverloadedStrings #-}

module Meowscript.Interpreter.Module
(
) where

import Meowscript.Data.Ref
import Meowscript.Parser.AST
import qualified Data.Text as Text
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Meowscript.Utils.Types

newtype Module = Module { getModule :: [Statement] }
newtype ModuleCache = ModuleCache { getCache :: Ref (HashMap.HashMap Text.Text Module) }

{- Standard Modules -}
----------------------------------------------------------------------------------------
stdModules :: HashSet.HashSet Text.Text
{-# INLINABLE stdModules #-}
stdModules = HashSet.fromList
    [ "std.meows"
    , "io.meows"
    , "time.meows"
    , "assert.meows"
    , "cat_tree.meows"
    , "hashmap.meows"
    , "hashset.meows"
    , "numbers.meows"
    , "exception.meows" ]

isStdModule :: FilePathT -> Bool
isStdModule = flip HashSet.member stdModules

{-
readStdFile :: FilePathT -> IO (Either Text.Text Text.Text)
{-# INLINABLE readStdFile #-}
readStdFile = readDataFile . Text.unpack . asStd
    where asStd = Text.append "std/"
-}
