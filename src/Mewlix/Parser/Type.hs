{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Parser.Type
( Parser
, parse
, ask
, asks
, local
, nested
, isNested
) where

import Mewlix.Parser.Nesting (Nesting, nested, isNested)
import Data.Text (Text)
import Data.Void (Void)
import Data.Bifunctor (first)
import Control.Applicative (Alternative)
import Control.Monad.Reader (MonadReader, Reader, MonadPlus, runReader, ask, asks, local)
import Text.Megaparsec (ParsecT, MonadParsec, runParserT, errorBundlePretty)

newtype Parser a = Parser { runParser :: ParsecT Void Text (Reader Nesting) a }
    deriving ( Functor
             , Applicative
             , Monad
             , Semigroup
             , Monoid
             , Alternative
             , MonadPlus
             , MonadFail
             , MonadReader Nesting
             , MonadParsec Void Text
             )

newtype ParseError = ParseError { getParseError :: String }
    deriving (Eq, Ord, Show)

parse :: FilePath -> Text -> Parser a -> Either ParseError a
parse path contents = prettifyError . (`runReader` mempty) . runParsecT . runParser
    where
        runParsecT parsec = runParserT parsec path contents
        prettifyError = first $ ParseError . errorBundlePretty
