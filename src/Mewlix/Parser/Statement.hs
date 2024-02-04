{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Statement
( root
, Nesting(..)
) where

import Mewlix.Abstract.AST
    ( Block(..)
    , Statement(..)
    , MewlixFunction(..)
    , MewlixClass(..)
    , Conditional(..)
    )
import Mewlix.Abstract.Key (Key(..))
import Mewlix.Abstract.Module (Module(..))
import Mewlix.Parser.Module (parseModuleKey)
import Mewlix.Parser.Primitive (parseKey, parseParams)
import Mewlix.Parser.Expression (declaration, expression, prettyExpr)
import Mewlix.Parser.Utils
    ( Parser
    , whitespaceLn
    , lexemeLn
    , parens
    , repeatChar
    )
import Mewlix.Parser.Keyword (keyword)
import Mewlix.Keywords.Types (SimpleKeyword(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Mewlix.Keywords.Constants as Keywords
import qualified Text.Megaparsec as Mega
import Control.Monad (when)
import Data.Maybe (fromMaybe)

root :: Parser Block
root = do
    let parser :: Parser Block
        parser = Block <$> Mega.many (statement Root)
    Mega.between whitespaceLn Mega.eof parser


{- Nesting -}
----------------------------------------------------------------
data Nesting =
      Root
    | Nested
    | NestedInLoop
    deriving (Eq, Ord, Show, Enum, Bounded)

-- The maximum nesting should always propagate.
-- The 'max' function is helpful: Root `max` Nested = Nested


{- Parse statements: -}
----------------------------------------------------------------
statement :: Nesting -> Parser Statement
statement nesting = Mega.choice $ map lexemeLn
    [ whileLoop     nesting
    , ifelse        nesting
    , declareVar    nesting
    , funcDef       nesting
    , returnKey     nesting
    , continueKey   nesting
    , breakKey      nesting
    , importKey     nesting
    , forEach       nesting
    , classDef      nesting
    , tryCatch      nesting
    , expressionStm nesting ]

block :: Nesting -> Maybe (Parser ()) -> Parser Block
block nesting customStop = do
    let stopPoint = fromMaybe (keyword Keywords.end) customStop
    let line = do
            Mega.notFollowedBy stopPoint
            statement nesting
    (fmap Block . Mega.many . lexemeLn) line

meowmeow :: Parser ()
meowmeow = Mega.choice
    [ keyword Keywords.end
    , fail "Block is unclosed!" ]


{- Expression -}
----------------------------------------------------------------
expressionStm :: Nesting -> Parser Statement
expressionStm  _ = ExpressionStatement <$> expression


{- Declaration -}
----------------------------------------------------------------
declareVar :: Nesting -> Parser Statement
declareVar nesting = do
    let bind = if nesting == Root then Binding else LocalBinding
    uncurry bind <$> declaration


{- While -}
----------------------------------------------------------------
whileLoop :: Nesting -> Parser Statement
whileLoop nesting = do
    keyword Keywords.while
    condition <- parens expression
    whitespaceLn
    body <- block (max nesting NestedInLoop) Nothing
    meowmeow
    return (WhileLoop condition body)


{- If Else -}
----------------------------------------------------------------
ifelse :: Nesting -> Parser Statement
ifelse nesting = do
    let nest = max nesting Nested
    let stopPoint = Mega.choice
            [ keyword Keywords.elif
            , keyword Keywords.else_
            , keyword Keywords.end  ]

    initialConditional <- do
        keyword Keywords.if_
        condition   <- parens expression
        whitespaceLn
        body        <- block nest (Just stopPoint)
        return (Conditional condition body)

    additonalConditionals <- Mega.many $ do
        keyword Keywords.elif
        condition   <- parens expression
        whitespaceLn
        body        <- block nest (Just stopPoint)
        return (Conditional condition body)

    elseBlock <- Mega.optional $ do
        keyword Keywords.else_
        whitespaceLn
        block nest Nothing

    meowmeow
    let conditionals = initialConditional :| additonalConditionals
    return (IfElse conditionals elseBlock)


{- Functions -}
----------------------------------------------------------------
func :: Parser MewlixFunction
func = do
    keyword Keywords.function
    name   <- parseKey
    params <- parseParams
    whitespaceLn
    body   <- block Nested Nothing
    meowmeow
    return (MewlixFunction name params body)

funcDef :: Nesting -> Parser Statement
funcDef _ = FunctionDef <$> func


{- Classes -}
----------------------------------------------------------------
classDef :: Nesting -> Parser Statement
classDef _ = do
    let (clowder, extends) = Keywords.clowder
    keyword clowder
    name        <- parseKey
    parent      <- Mega.optional (keyword extends >> parseKey)
    whitespaceLn
    methods     <- (Mega.many . lexemeLn) func
    constructor <- getConstructor methods
    (return . ClassDef) (MewlixClass name parent constructor methods)

getConstructor :: [MewlixFunction] -> Parser (Maybe MewlixFunction)
getConstructor funcs = do
    let wake = Key (unwrapKeyword Keywords.constructor)
    let constructors = filter ((== wake) . funcName) funcs
    when (length constructors > 1)
        (fail "Class cannot have more than one constructor!")
    let constructor = case constructors of
            []    -> Nothing
            (x:_) -> Just x
    return constructor


{- Return -}
----------------------------------------------------------------
returnKey :: Nesting -> Parser Statement
returnKey _ = do
    keyword Keywords.ret
    Return <$> expression

{- Continue -}
----------------------------------------------------------------
continueKey :: Nesting -> Parser Statement
continueKey nesting = do
    keyword Keywords.catnap
    when (nesting < NestedInLoop)
        (fail "Cannot use loop keyword outside loop!")
    return Continue


{- Break -}
----------------------------------------------------------------
breakKey :: Nesting -> Parser Statement
breakKey nesting = do
    keyword Keywords.break
    when (nesting < NestedInLoop)
        (fail "Cannot use loop keyword outside loop!")
    return Break

{- For Loop -}
----------------------------------------------------------------
forEach :: Nesting -> Parser Statement
forEach nesting = do
    keyword Keywords.forEach
    iter <- prettyExpr
    repeatChar '!'
    keyword Keywords.thenDo
    repeatChar '!'
    key  <- parseKey
    whitespaceLn
    body <- block (max nesting NestedInLoop) Nothing
    meowmeow
    return (ForEachLoop iter key body)


{- Import -}
----------------------------------------------------------------
importKey :: Nesting -> Parser Statement
importKey nesting = do
    keyword Keywords.takes
    path <- parseModuleKey
    name <- Mega.optional (keyword Keywords.alias >> parseKey)
    when (nesting > Root)
        (fail "Import statements cannot be nested!")
    return $ ImportStatement (Module path name)


{- Watch/Catch -}
----------------------------------------------------------------
tryCatch :: Nesting -> Parser Statement
tryCatch nesting = do
    let localNest = max nesting Nested

    keyword Keywords.try
    whitespaceLn
    try_    <- block localNest (Just $ keyword Keywords.catch)

    keyword Keywords.catch
    key_    <- Mega.optional parseKey
    whitespaceLn
    catch_  <- block localNest Nothing

    meowmeow
    return (TryCatch try_ key_ catch_)
