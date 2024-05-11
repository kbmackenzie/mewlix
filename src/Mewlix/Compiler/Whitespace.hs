{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Compiler.Whitespace
( joinLines
) where

import Mewlix.Compiler.Transpiler (Transpiler, asks, pretty)
import Data.Text (Text)
import qualified Data.Text as Text

joinLines :: [Transpiler Text] -> Transpiler Text
joinLines lines_ = do
    prettify <- asks pretty
    let cat = if prettify
        then Text.intercalate "\n"
        else Text.concat
    fmap cat (sequence lines_)
