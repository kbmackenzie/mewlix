{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Utils.Yaml
( parseYaml
, toPrettyYaml
) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Yaml (decodeEither', FromJSON, ToJSON, prettyPrintParseException)
import Data.Yaml.Pretty (encodePretty, setConfCompare, defConfig)
import Data.Bifunctor (first)

parseYaml :: (FromJSON a) => ByteString -> Either String a
parseYaml = first prettyPrintParseException . decodeEither'

toPrettyYaml :: (ToJSON a) => (Text -> Text -> Ordering) -> a -> ByteString
toPrettyYaml = encodePretty . flip setConfCompare defConfig
