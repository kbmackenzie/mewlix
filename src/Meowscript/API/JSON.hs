{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.API.JSON
( toJSON
, prettyJSON
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

prettyJSON :: Prim -> IO Text.Text
prettyJSON = prettyJSON' 0

makeIndent :: Int -> Text.Text
makeIndent i = Text.replicate (i * 2) " "

around' :: Char -> Char -> Int -> Text.Text -> Text.Text
around' open close i = (`Text.append` close') . Text.append open'
    where open'  = Text.pack [ open, '\n' ]
          close' = Text.concat [ "\n", makeIndent i, Text.singleton close ]

braces' :: Int -> Text.Text -> Text.Text
braces' = around' '{' '}'

brackets' :: Int -> Text.Text -> Text.Text
brackets' = around' '[' ']'

prettyJSON' :: Int -> Prim -> IO Text.Text
prettyJSON' i (MeowList xs) = do
    let i' = succ i
    let items = mapM (prettyJSON' i') xs
    let indentation = Text.append $ makeIndent i'
    brackets' i . Text.intercalate ",\n" . map indentation <$> items
prettyJSON' i (MeowObject x) = do
    let unpack (key, ref) = (key,) <$> readIORef ref
    let i' = succ i
    let makeKey = (`Text.append` ": ") . quotes . Text.concatMap escapeChar
    let printPair (key, value) = Text.append (makeKey key) <$> prettyJSON' i' value
    let indentation = Text.append $ makeIndent i'
    let pairs = mapM (unpack >=> printPair) (Map.toList x)
    braces' i . Text.intercalate ",\n" . map indentation <$> pairs
prettyJSON' _ prim = toJSON prim
