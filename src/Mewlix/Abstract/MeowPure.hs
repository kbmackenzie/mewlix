{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Abstract.MeowPure
( MeowPure(..)
, prettify
, compareMeow
, sortMeow
) where

import Mewlix.Data.Stack (Stack)
import Data.Text (Text)
import Mewlix.Utils.Show (showT)
import Mewlix.Parser.Keywords (meowBox, meowNil)
import Mewlix.Data.ToBool (ToBool(..))
import Mewlix.Utils.List (listCompareM)
import Data.HashMap.Strict (HashMap)
import Data.Char (toLower)
import qualified Data.Text as Text
import qualified Control.Monad.ListM as ListM
import qualified Data.HashMap.Strict as HashMap
import qualified Mewlix.Data.Stack as Stack

{- A pure variant of 'MeowPrim', which can be used for simpler and faster primitive operations.
 - It makes no use of IORefs or the IO monad.
 - Because of its purity, I can declare proper class instances for it. -}

data MeowPure =
      MeowPureInt Int
    | MeowPureFloat Double
    | MeowPureString Text
    | MeowPureBool Bool
    | MeowPureStack (Stack MeowPure)
    | MeowPureBox (HashMap Text MeowPure)
    | MeowPureNil
    | MeowPureLabel Text

instance ToBool MeowPure where
    toBool MeowPureNil          = False
    toBool (MeowPureBool b)     = b
    toBool (MeowPureStack s)    = (not . Stack.null) s
    toBool (MeowPureString s)   = (not . Text.null) s
    toBool _                    = False


{- Utils -}
----------------------------------------------------------
escapeChar :: Char -> Text
escapeChar c = case c of
    '\n' -> "\\n"
    '\f' -> "\\f"
    '\b' -> "\\b"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '/'  -> "\\/"
    '"'  -> "\\\""
    '\\' -> "\\\\"
    _    -> Text.singleton c

escapeString :: Text -> Text
escapeString = Text.concatMap escapeChar

prettyString :: Text -> Text
prettyString str = Text.concat [ "\"", escapeString str, "\"" ]

prettyList :: [MeowPure] -> Text
prettyList xs = do
    let items = Text.intercalate ", " (map prettify xs)
    Text.intercalate " " [ "[", items, "]" ]

prettyBox :: HashMap Text MeowPure -> Text
prettyBox box = do
    let prettyPair :: (Text, MeowPure) -> Text
        prettyPair (key, value) = Text.concat [ escapeString key, ": ", prettify value ]
    let pairs = (Text.intercalate ", " . map prettyPair . HashMap.toList) box
    Text.intercalate " " [ meowBox, "[", pairs, "]" ]

{- Prettify -}
----------------------------------------------------------
prettify :: MeowPure -> Text
prettify (MeowPureInt n) = showT n
prettify (MeowPureFloat n) = showT n
prettify (MeowPureString s) = prettyString s
prettify (MeowPureBool b) = (Text.pack . map toLower . show) b
prettify MeowPureNil = meowNil
prettify (MeowPureStack a) = (prettyList . Stack.toList) a
prettify (MeowPureBox a) = prettyBox a
prettify (MeowPureLabel l) = Text.concat [ "<", l, ">" ]

{- Comparison -}
----------------------------------------------------------
compareMeow :: MeowPure -> MeowPure -> Either Text Ordering
-- Numbers:
MeowPureInt a     `compareMeow` MeowPureInt b     = Right (a `compare` b)
MeowPureFloat a   `compareMeow` MeowPureInt b     = Right (a `compare` fromIntegral b)
MeowPureInt a     `compareMeow` MeowPureFloat b   = Right (fromIntegral a `compare` b)
MeowPureFloat a   `compareMeow` MeowPureFloat b   = Right (a `compare` b)
-- Strings:
MeowPureString a  `compareMeow` MeowPureString b  = Right (a `compare` b)
-- Nil:
MeowPureNil       `compareMeow` MeowPureNil       = Right EQ
MeowPureNil       `compareMeow` _                 = Right LT
_                 `compareMeow` MeowPureNil       = Right GT
-- Booleans:
MeowPureBool a    `compareMeow` MeowPureBool b    = Right (a `compare` b)
MeowPureBool a    `compareMeow` b                 = Right (a `compare` toBool b)
a                 `compareMeow` MeowPureBool b    = Right (toBool a `compare` b)
-- Stacks:
MeowPureStack a   `compareMeow` MeowPureStack b   = Stack.compareM compareMeow a b
-- Boxes:
MeowPureBox a     `compareMeow` MeowPureBox b     = do
    let comparePair :: (Text, MeowPure) -> (Text, MeowPure) -> Either Text Ordering
        comparePair (keyA, valueA) (keyB, valueB) = do
            let keys = keyA `compare` keyB
            fmap (keys `mappend`) (compareMeow valueA valueB)
    listCompareM comparePair (HashMap.toList a) (HashMap.toList b)
-- Anything else:
a                 `compareMeow` b                 = (Left . Text.concat)
    [ "Can't compare values: ", prettify a, ", ", prettify b ]

{- Sorting -}
----------------------------------------------------------
sortMeow :: [MeowPure] -> Either Text [MeowPure]
sortMeow = ListM.sortByM compareMeow
