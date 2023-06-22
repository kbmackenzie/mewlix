{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Regex.Match
( runRegex
) where

import Meowscript.Regex.Core
import Meowscript.Regex.Parser
import qualified Data.Text as Text
import Control.Monad.State (gets, put, get, modify)
import Control.Applicative (empty, many, some, (<|>), optional, asum)
import qualified Data.List as List
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Control.Monad (void)

epsilon :: StateMachine a
epsilon = empty

anyChar :: StateMachine Char
anyChar = gets Text.uncons >>= \case
    Nothing     -> epsilon
    Just (x, xs) -> put xs >> return x

sandbox :: StateMachine a -> StateMachine a
sandbox action = do
    state <- get
    action <* put state

attempt :: StateMachine a -> StateMachine a
attempt action = do
    state <- get
    action <|> (put state >> empty)

matchChar :: (Char -> Bool) -> StateMachine Char
matchChar predicate = gets Text.uncons >>= \case
    Nothing -> epsilon
    (Just (x, xs)) -> if predicate x
        then put xs >> return x
        else epsilon

matchText :: Text.Text -> StateMachine Text.Text
matchText v = gets (Text.stripPrefix v) >>= \case
    Nothing  -> epsilon
    (Just xs) -> put xs >> return v

matchAhead :: [RegexAST] -> StateMachine Text.Text -> StateMachine Text.Text
matchAhead xs action = do
    t <- action
    t <$ sandbox (consumeInput xs)

matchRepeat :: [RegexAST] -> StateMachine Text.Text -> StateMachine Text.Text
matchRepeat xs action = do
    let matcher = matchAhead xs action
    Text.concat <$> many (attempt matcher)

{- States -}
matchStar :: RegexAST -> [RegexAST] -> StateMachine Text.Text
matchStar x xs = matchRepeat xs (execute x xs)

matchPlus :: RegexAST -> [RegexAST] -> StateMachine Text.Text
matchPlus x xs = do 
    t <- execute x xs
    Text.append t <$> matchRepeat xs (execute x xs)

matchQues :: RegexAST -> [RegexAST] -> StateMachine (Maybe Text.Text)
matchQues x xs = (optional . matchAhead xs) (execute x xs)


{- Transitions -}

execute :: RegexAST -> [RegexAST] -> StateMachine Text.Text
execute state xs = case state of
    Verbatim x          -> matchText x
    AnyChar             -> Text.singleton <$> anyChar
    ZeroOrOne x         -> fromMaybe Text.empty <$> matchQues x xs
    ZeroOrMore x        -> matchStar x xs
    OneOrMore x         -> matchPlus x xs
    CharacterClass f    -> Text.singleton <$> matchChar (getPredicate f)
    _ -> undefined

consumeInput :: [RegexAST] -> StateMachine [Text.Text]
consumeInput [] = return []
consumeInput (x:xs) = (:) <$> execute x xs <*> consumeInput xs

runRegex :: Text.Text -> Text.Text -> Either Text.Text [Text.Text]
runRegex pattern input = case parseRegex pattern of 
    (Left x)  -> Left ("Parse error: " `Text.append` x)
    (Right xs) -> case runStateMachine input (consumeInput xs) of
        (Nothing, rest) -> Left $ "Not a match!: " `Text.append` rest
        (Just txt, _)    -> Right txt
