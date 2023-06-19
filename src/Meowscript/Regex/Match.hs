{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Regex.Match
( runMatch
) where

import Meowscript.Regex.Core
import Meowscript.Regex.Parser
import qualified Data.Text as Text
import Control.Monad.State (gets, put, get)
import Control.Applicative (empty, many, some, (<|>), optional)
import Data.Maybe (isNothing)
import qualified Data.List as List

runMatch :: Text.Text -> Text.Text -> Either Text.Text [Text.Text]
runMatch pattern input = case parseRegex pattern of 
    (Left x)  -> Left ("Parse error: " `Text.append` x)
    (Right x) -> case runMatcher input (matchAll x) of
        Nothing -> Left "Not a match!"
        (Just xs) -> Right xs

matchDot :: Matcher Char
matchDot = gets Text.uncons >>= \case
    Nothing -> empty
    (Just (x, xs)) -> put xs >> return x

matchText :: Text.Text -> Matcher Text.Text
matchText x = gets (Text.stripPrefix x) >>= \case
    Nothing -> empty
    (Just xs) -> put xs >> return x

matchStar :: MeowRegex -> [MeowRegex] -> Matcher [Text.Text]
matchStar x xs = many (safeMatch (matchToken x xs) xs)

matchPlus :: MeowRegex -> [MeowRegex] -> Matcher [Text.Text]
matchPlus x xs = some (safeMatch (matchToken x xs) xs)

matchQues :: MeowRegex -> [MeowRegex] -> Matcher (Maybe Text.Text)
matchQues x xs = optional (safeMatch (matchToken x xs) xs)

safeMatch :: Matcher a -> [MeowRegex] -> Matcher a
safeMatch matcher xs = do
    state <- get
    x <- matcher
    state' <- get
    x <$ (matchTokens xs >> put state') <|> (put state >> empty)

matchToken :: MeowRegex -> [MeowRegex] -> Matcher Text.Text
matchToken (Verbatim x) _ = matchText x
matchToken AnyChar _ = Text.singleton <$> matchDot
matchToken (OneOrMore x) xs = Text.concat <$> matchPlus x xs
matchToken (ZeroOrMore x) xs = Text.concat <$> matchStar x xs
matchToken (ZeroOrOne x) xs = matchQues x xs >>= \case
    Nothing  -> return Text.empty
    (Just y) -> return y
matchToken _ _ = undefined

matchTokens :: [MeowRegex] -> Matcher [Text.Text]
matchTokens [] = return []
matchTokens (x:xs) = (:) <$> matchToken x xs <*> matchTokens xs

matchAll :: [MeowRegex] -> Matcher [Text.Text]
matchAll xs = matchTokens xs <* (gets Text.null >>= \x -> if x then return [] else empty)
