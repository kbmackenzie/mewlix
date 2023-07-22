{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.API.JSON
( toJSON
) where

import Meowscript.Core.AST
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Meowscript.Utils.Show
import Control.Monad ((>=>))
import Data.IORef (readIORef)

around :: Char -> Char -> Text.Text -> Text.Text
around open close = (`Text.snoc` close) . Text.cons open

braces :: Text.Text -> Text.Text
braces = around '{' '}'

brackets :: Text.Text -> Text.Text
brackets = around '[' ']'

quotes :: Text.Text -> Text.Text
quotes = around '"' '"'

escapeChar :: Char -> Text.Text
escapeChar c = case c of
    '\t' -> "\\t"
    '\n' -> "\\n"
    '\f' -> "\\f"
    '\r' -> "\\r"
    '\b' -> "\\b"
    '\\' -> "\\\\"
    '/'  -> "\\/"
    _    -> Text.singleton c

toJSON :: Prim -> IO Text.Text
toJSON (MeowInt x) = (return . showT) x
toJSON (MeowDouble x) = (return . showT) x
toJSON (MeowBool x) = (return . Text.toLower . showT) x
toJSON (MeowString x) = (return . quotes . Text.concatMap escapeChar) x
toJSON (MeowList xs) = brackets . Text.intercalate ", " <$> mapM toJSON xs
toJSON MeowLonely = return "null"
toJSON (MeowObject x) = do
    let unpack (key, ref) = (key,) <$> readIORef ref
    let makeKey = (`Text.append` ": ") . quotes . Text.concatMap escapeChar
    let printPair (key, value) = Text.append (makeKey key) <$> toJSON value
    let xs = Map.toList x
    braces . Text.intercalate ", " <$> mapM (unpack >=> printPair) xs
toJSON _ = return "null"
