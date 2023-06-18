{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Regex.Match
(
) where

import Meowscript.Regex.Core
import Meowscript.Regex.Parser
import qualified Data.Text as Text
import Control.Monad.State (gets, put)
import Data.Functor ((<&>))
import Control.Monad (void)

matchDot :: Interpreter Match
matchDot = gets Text.uncons >>= \case
    Nothing -> return Nothing
    (Just (x, xs)) -> put xs >> (return . Just . Text.singleton) x

biteInto :: Text.Text -> [Text.Text]
biteInto txt = case Text.uncons txt of
    Nothing         -> []
    (Just (_, xs))  -> xs : biteInto xs

matchPlus :: MeowRegex -> Interpreter Match
matchPlus token = matchToken token

matchToken :: MeowRegex -> Interpreter Match
matchToken = undefined
