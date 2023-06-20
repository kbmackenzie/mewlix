{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Regex.Match
( runMatch
) where

import Meowscript.Regex.Core
import Meowscript.Regex.Parser
import qualified Data.Text as Text
import Control.Monad.State (gets, put, get)
import Control.Applicative (empty, many, some, (<|>), optional, asum)
import qualified Data.List as List
import Data.Char (isDigit, isSpace)
import Control.Monad (void)

epsilon :: Matcher a
epsilon = empty

runMatch :: Text.Text -> Text.Text -> Either Text.Text [Text.Text]
runMatch pattern input = case parseRegex pattern of 
    (Left x)  -> Left ("Parse error: " `Text.append` x)
    (Right x) -> case runMatcher input (matchAll x) of
        (Nothing, rest) -> Left $ "Not a match!: " `Text.append` rest
        (Just xs, _)    -> Right xs

anyChar :: Matcher Char
anyChar = gets Text.uncons >>= \case
    Nothing -> epsilon
    (Just (x, xs)) -> put xs >> return x

matchText :: Text.Text -> Matcher Text.Text
matchText x = gets (Text.stripPrefix x) >>= \case
    Nothing -> epsilon
    (Just xs) -> put xs >> return x

matchStar :: MeowRegex -> [MeowRegex] -> Matcher [Text.Text]
matchStar x xs = many (safeMatch' x xs)

matchPlus :: MeowRegex -> [MeowRegex] -> Matcher [Text.Text]
matchPlus x xs = some (safeMatch' x xs)

matchQues :: MeowRegex -> [MeowRegex] -> Matcher (Maybe Text.Text)
matchQues x xs = optional (safeMatch' x xs)

safeMatch' :: MeowRegex -> [MeowRegex] -> Matcher Text.Text
safeMatch' x xs = safeMatch (matchToken x xs <* lookAhead (matchTokens xs))

lookAhead :: Matcher a -> Matcher a
lookAhead match = do
    state <- get
    match <* put state

safeMatch :: Matcher a -> Matcher a
safeMatch match = do
    state <- get
    match <|> (put state >> epsilon)

{-
safeMatch :: Matcher a -> [MeowRegex] -> Matcher a
safeMatch matcher xs = do
    state <- get
    x <- matcher
    state' <- get
    x <$ (matchTokens xs >> put state') <|> (put state >> epsilon)
-}

matchCount :: Int -> MeowRegex -> [MeowRegex] -> Matcher [Text.Text]
matchCount n x xs = do
    tokens <- many (matchToken x xs)
    if length tokens < n
        then epsilon
        else return tokens

matchRange :: Maybe Int -> Maybe Int -> MeowRegex -> [MeowRegex] -> Matcher [Text.Text]
matchRange (Just start) (Just end) x xs = do
    tokens <- many (matchToken x xs)
    if length tokens < start || length tokens > end then epsilon else return tokens
matchRange (Just start) Nothing x xs = do
    tokens <- many (matchToken x xs)
    if length tokens < start then epsilon else return tokens
matchRange Nothing (Just end) x xs = do
    tokens <- many (matchToken x xs)
    if length tokens > end then epsilon else return tokens
matchRange Nothing Nothing _ _ = epsilon

matchChar :: Bool -> (Char -> Bool) -> Matcher Char
matchChar bool fn = gets Text.uncons >>= \case
    Nothing -> epsilon
    (Just (x, xs)) -> if predicate x
        then put xs >> return x
        else epsilon
    where predicate = if bool then fn else not . fn

classMatch :: Bool -> [Predicate] -> Matcher Char
classMatch b = foldr ((<|>) . matchChar b . getPredicate) epsilon

{- Match Tokens -}
matchToken :: MeowRegex -> [MeowRegex] -> Matcher Text.Text
matchToken (Verbatim x) _ = matchText x
matchToken AnyChar _ = Text.singleton <$> anyChar
matchToken (Digit b) _ = Text.singleton <$> matchChar b isDigit
matchToken (WordChar b) _ = Text.singleton <$> matchChar b isWordChar
matchToken (Whitespace b) _ = Text.singleton <$> matchChar b isSpace

matchToken LineStart _ = epsilon
matchToken LineEnd _ = gets Text.null
    >>= (\x -> if x then return Text.empty else epsilon)

matchToken (OneOrMore x) xs = Text.concat <$> matchPlus x xs
matchToken (ZeroOrMore x) xs = Text.concat <$> matchStar x xs
matchToken (ZeroOrOne x) xs = matchQues x xs >>= \case
    Nothing  -> return Text.empty
    (Just y) -> return y
matchToken (Count n x) xs = Text.concat <$> matchCount n x xs
matchToken (CountRange a b x) xs = Text.concat <$> matchRange a b x xs

matchToken (CharacterClass x ps) _ = Text.singleton <$> classMatch x ps
matchToken (CaptureGroup _ ys) xs = Text.concat <$> matchGroup ys xs
matchToken (Alternation as bs) _ = Text.concat <$> do
    state <- get
    matchTokens as <|> (put state >> matchTokens bs)

matchGroup :: [MeowRegex] -> [MeowRegex] -> Matcher [Text.Text]
matchGroup [] _ = return []
matchGroup (y:ys) xs = (:) <$> matchToken y (ys ++ xs) <*> matchGroup ys xs


{- Match Tokens -}
matchTokens :: [MeowRegex] -> Matcher [Text.Text]
matchTokens [] = return []
matchTokens (x:xs) = (:) <$> matchToken x xs <*> matchTokens xs

matchAll :: [MeowRegex] -> Matcher [Text.Text]
matchAll [] = return []
matchAll (LineStart:xs) = matchTokens xs
matchAll xs = let rec toks = safeMatch (matchTokens toks) <|> (anyChar >> matchAll toks)
    in rec xs

matchStart :: [MeowRegex] -> Matcher [Text.Text]
matchStart (LineStart:xs) = matchTokens xs
matchStart xs = matchAll xs
