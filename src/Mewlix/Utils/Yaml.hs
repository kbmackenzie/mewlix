{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Utils.Yaml
( readYaml
, prettyYaml
) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Yaml (decodeEither', FromJSON, ToJSON, ParseException)
import Data.Yaml.Pretty (encodePretty, setConfCompare, defConfig)

readYaml :: (MonadIO m, FromJSON a) => FilePath -> m (Either ParseException a)
readYaml = fmap decodeEither' . liftIO . ByteString.readFile

prettyYaml :: (ToJSON a) => (Text -> Text -> Ordering) -> a -> ByteString
prettyYaml = encodePretty . flip setConfCompare defConfig
