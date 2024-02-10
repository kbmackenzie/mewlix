{-# LANGUAGE FlexibleContexts #-}

module Mewlix.Utils.Yaml
( readYaml
) where

import qualified Data.ByteString as ByteString
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Yaml (decodeEither', FromJSON, ParseException)

readYaml :: (MonadIO m, FromJSON a) => FilePath -> m (Either ParseException a)
readYaml = fmap decodeEither' . liftIO . ByteString.readFile
