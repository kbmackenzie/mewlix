{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Regex.Match
( runRegex
) where

import Meowscript.Regex.Core
import Meowscript.Regex.Parser
import qualified Data.Text as Text
import Control.Monad.State (gets, put, get, modify)
import Control.Applicative (empty, many, (<|>), optional)
import Data.Maybe (fromMaybe)

{- Helpers -}
----------------------------------------------------------
failed :: StateMachine a
failed = empty

lookahead :: StateMachine a -> StateMachine a
lookahead action = do
    state <- get
    action <* put state

try :: StateMachine a -> StateMachine a
try action = do
    state <- get
    action <|> (put state >> empty)


{- Running the AST -}
----------------------------------------------------------
anyChar :: StateMachine Char
anyChar = gets Text.uncons >>= \case
    Nothing         -> failed
    Just (x, xs)    -> put xs >> return x

matchChar :: (Char -> Bool) -> StateMachine Char
matchChar predicate = gets Text.uncons >>= \case
    Nothing         -> failed
    (Just (x, xs))  -> if predicate x
        then put xs >> return x
        else failed

matchText :: Text.Text -> StateMachine Text.Text
matchText v = gets (Text.stripPrefix v) >>= \case
    Nothing     -> failed
    (Just xs)   -> put xs >> return v

matchAhead :: [RegexAST] -> StateMachine Text.Text -> StateMachine Text.Text
matchAhead xs action = do
    t <- action
    t <$ lookahead (consumeInput xs)

matchRepeat :: [RegexAST] -> StateMachine Text.Text -> StateMachine Text.Text
matchRepeat xs action = do
    let matcher = matchAhead xs action
    Text.concat <$> many (try matcher)

matchStar :: RegexAST -> [RegexAST] -> StateMachine Text.Text
matchStar x xs = matchRepeat xs (execute x xs)

matchPlus :: RegexAST -> [RegexAST] -> StateMachine Text.Text
matchPlus x xs = do 
    t <- execute x xs
    Text.append t <$> matchRepeat xs (execute x xs)

matchQues :: RegexAST -> [RegexAST] -> StateMachine (Maybe Text.Text)
matchQues x xs = (optional . try) $ matchAhead xs (execute x xs)

matchAlt :: [RegexAST] -> [RegexAST] -> StateMachine [Text.Text]
matchAlt a b = (try . consumeInput) a <|> consumeInput b


{- Running the Machine -}
----------------------------------------------------------
execute :: RegexAST -> [RegexAST] -> StateMachine Text.Text
execute state xs = case state of
    Verbatim x          -> matchText x
    AnyChar             -> Text.singleton <$> anyChar
    ZeroOrOne x         -> fromMaybe Text.empty <$> matchQues x xs
    ZeroOrMore x        -> matchStar x xs
    OneOrMore x         -> matchPlus x xs
    CharacterClass f    -> Text.singleton <$> matchChar (getPredicate f)
    Alternation a b     -> Text.concat <$> matchAlt a b
    _ -> undefined

consumeInput :: [RegexAST] -> StateMachine [Text.Text]
consumeInput [] = return []
consumeInput (x:xs) = (:) <$> execute x xs <*> consumeInput xs

runRegex :: Text.Text -> Text.Text -> Either Text.Text [Text.Text]
runRegex pattern input = case parseRegex pattern of 
    (Left x)  -> Left ("Parse error: " `Text.append` x)
    (Right xs) -> case runStateMachine input (consumeInput xs) of
        (Nothing, rest) -> Left $ "Not a match!: " `Text.append` rest
        (Just txt, _)   -> Right txt
