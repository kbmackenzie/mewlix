{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Mewlix.Parser.Type
( Parser
, parse
, ask
, asks
, local
, Nesting
, NestingFlag(..)
, addNesting
, nested
, noNesting
, defineNesting
, FileContent
, ParseError
) where

import Mewlix.Parser.Nesting
    ( Nesting
    , NestingFlag(..)
    , addNesting
    , nested
    , noNesting
    , defineNesting
    )
import Data.Text (Text)
import Data.Void (Void)
import Data.Bifunctor (first)
import Control.Monad (MonadPlus)
import Control.Applicative (Alternative)
import Control.Monad.Reader (MonadReader, Reader, runReader, ask, asks, local)
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

type FileContent = Text
type ParseError = String

parse :: FilePath -> Text -> Parser a -> Either ParseError a
parse path contents = prettifyError . (`runReader` mempty) . runParsecT . runParser
    where
        runParsecT parsec = runParserT parsec path contents
        prettifyError = first errorBundlePretty
