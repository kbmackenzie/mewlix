{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Utils.Yaml
( fromYaml
, toPrettyYaml
) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Yaml (decodeEither', FromJSON, ToJSON, prettyPrintParseException)
import Data.Yaml.Pretty (encodePretty, setConfCompare, defConfig)
import Data.Bifunctor (first)

fromYaml :: (FromJSON a) => ByteString -> Either String a
fromYaml = first prettyPrintParseException . decodeEither'

toPrettyYaml :: (ToJSON a) => (Text -> Text -> Ordering) -> a -> ByteString
toPrettyYaml = encodePretty . flip setConfCompare defConfig
