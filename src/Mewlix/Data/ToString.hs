{-# LANGUAGE FlexibleInstances #-}

module Mewlix.Data.ToString
( ToString(..)
) where

import Data.String (IsString(..))
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

{- *Question*: "But what's the difference between this and 'Show'?"
 - Answer*: 'Show' is for pretty-printing.
 - 'ToString' is meant for conversion between Haskell's *string types*.
 -
 - Instances of the 'Show' typeclass for Haskell's string types (String, Text, Bytestring, etc)
 - all return a ***pretty-printed*** representation of the string.
 -
 - For example, the 'Show' instance for Text gives you a pretty printed string:
 - show (Text.pack "hello\n") returns "\"hello\\\n""; quotation marks are included and the newline is escaped.
 -
 - A similar thing happens with String and Bytestring.
 -
 - In contrast, the ToString instance of Text is gonna be just... 'Text.unpack'.
 - Thus:
 - toString (Text.pack "hello\n") returns "hello\n", no quotation marks or escape sequences.
 -
 - The 'ToString' typeclass is just an util for me in order to have seamless conversion between
 - Haskell's many string types. -}

class ToString a where
    toString :: a -> String

    -- Generic string conversion.
    -- Needs type casting with :: often.
    stringConvert :: (IsString b) => a -> b
    stringConvert = fromString . toString

instance ToString String where
    toString = id

instance ToString Text.Text where
    toString = Text.unpack

instance ToString ByteString where
    toString = UTF8.toString
