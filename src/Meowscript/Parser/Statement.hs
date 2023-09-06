{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Meowscript.Parser.Statement
( statements
, root
) where

import Meowscript.Parser.AST
import Meowscript.Parser.Expr
import Meowscript.Parser.Utils
import Meowscript.Parser.Keywords
import Meowscript.Parser.Prim
import qualified Meowscript.Data.Stack as Stack
import Text.Megaparsec ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void)

root :: Parser Block
root = do
    let parser = (Mega.many . lexemeLn) (statements Root)
    Stack.fromList <$> Mega.between whitespaceLn Mega.eof parser

{- Nesting -}
----------------------------------------------------------------
data Nesting =
      Root
    | Nested
    | NestedInLoop
    deriving (Eq, Ord, Show, Enum, Bounded)

-- to resolve nesting, do
-- a `max` b
-- the maximum nesting always propagates! for example
-- root `max` nestedinloop = nestedinloop


{- Parse statements: -}
----------------------------------------------------------------
statements :: Nesting -> Parser Statement
statements n = Mega.choice
    [ parseWhile n               <??> meowWhile
    , parseFor n                 <??> (\(x,_,_) -> x) meowFor
    , parseIfElse n              <??> meowIf
    , parseFunc n                <??> meowCatface
    , parseDeclaration n         <??> meowLocal
    , parseReturn n              <??> meowReturn
    , parseContinue n            <??> meowContinue
    , parseBreak n               <??> meowBreak
    , parseWhen n                <??> fst meowWhen
    , parseImport n              <??> fst meowTakes
    , parseTryCatch n            <??> Text.concat [ meowTry, "/", meowCatch ]
    , parseExpression n          <??> ("expression" :: String) ]

parseEnd :: Parser ()
parseEnd = whitespace >> (void . keyword) meowEnd
    <|> fail (concat ["Expected '", Text.unpack meowEnd, "': Block is unclosed instead!"])

parensExpression :: Parser Expr
parensExpression = (lexeme . parens) (whitespace >> parseExpr)

{- Expression -}
----------------------------------------------------------------
parseExpression :: Nesting -> Parser Statement
parseExpression _ = whitespace >> StmtExpr <$> parseExpr


{- Declaration -}
----------------------------------------------------------------
parseDeclaration :: Nesting -> Parser Statement
parseDeclaration _ = uncurry StmtDeclaration <$> declaration


{- While -}
----------------------------------------------------------------
parseWhile:: Nesting -> Parser Statement
parseWhile nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowWhile
    whitespace
    condition <- parensExpression
    let nest = max nesting NestedInLoop
    let makeBody = (<$ parseEnd) . StmtWhile condition . Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody (statements nest)


{- If Else -}
----------------------------------------------------------------
data MeowIf = MeowIf Expr Block deriving (Show)

parseIf :: Nesting -> Parser (Block -> Statement)
parseIf nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowIf
    condition <- parensExpression
    let nest = max nesting Nested
    let makeBody = return . StmtIfElse condition. Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody (statements nest)

parseElse :: Nesting -> Parser Block
parseElse nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowElse
    let nest = max nesting Nested
    return $ Lexer.IndentMany Nothing (return . Stack.fromList) (statements nest)

parseIfElse :: Nesting -> Parser Statement
parseIfElse nesting = do
    ifs         <- Mega.some (parseIf nesting)
    elseBlock   <- Mega.try (parseElse nesting) <|> return Stack.empty
    let f :: (Block -> Statement) -> (Block -> Statement) -> (Block -> Statement)
        f x acc = x . Stack.singleton . acc
    let foldedIf = foldr1 f ifs
    parseEnd
    return $ foldedIf elseBlock


{- Functions -}
----------------------------------------------------------------
funParams :: Parser Params
funParams = (lexeme . parens) $ whitespace >> Stack.fromList <$> sepByComma (bilexeme keyText)

parseFunc :: Nesting -> Parser Statement
parseFunc _ = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . specialSymbol) meowCatface
    name <- lexeme funName
    args <- funParams
    let makeBody = (<$ parseEnd) . StmtFuncDef name args . Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody (statements Nested)

funName :: Parser Expr
funName = whitespace >> lexeme exprFnKey

{- Return -}
----------------------------------------------------------------
parseReturn :: Nesting -> Parser Statement
parseReturn _ = lexeme $ do
    whitespace
    (void . keyword) meowReturn
    StmtReturn <$> parseExpr


{- Continue -}
----------------------------------------------------------------
parseContinue :: Nesting -> Parser Statement
parseContinue nesting = lexeme $ do
    whitespace
    (void . keyword) meowContinue
    case nesting of
        NestedInLoop -> return StmtContinue
        _            -> fail $ loopKeywordOutsideLoop meowContinue


{- Break -}
----------------------------------------------------------------
parseBreak :: Nesting -> Parser Statement
parseBreak nesting = lexeme $ do
    whitespace
    (void . keyword) meowBreak
    case nesting of
        NestedInLoop -> return StmtBreak
        _            -> fail $ loopKeywordOutsideLoop meowBreak


{- For Loop -}
----------------------------------------------------------------
parseFor :: Nesting -> Parser Statement
parseFor nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    let (start, middle, end) = meowFor
    let lifted = (lexeme . parens) liftedExpr
    let tryFirst = tryKeyword start >> lifted
    let getExp name = keyword name >> parensExpression
    (a, b, c) <- (,,) <$> tryFirst <*> getExp middle <*> getExp end
    let expressions = (a, b, c)
    let nest = max nesting NestedInLoop
    let makeBody = (<$ parseEnd) . StmtFor expressions . Stack.fromList
    return $ Lexer.IndentMany Nothing makeBody (statements nest)


{- Import -}
----------------------------------------------------------------
parseImport :: Nesting -> Parser Statement
parseImport nesting = lexeme $ do
    let (start, end) = meowTakes
    (void . tryKeyword) start
    (PrimStr filepath) <- lexeme parseStr
    let key = (void . tryKeyword) end >> lexeme keyText
    maybeKey <- Mega.optional key
    case nesting of
        Root -> return (StmtImport filepath maybeKey)
        _    -> fail "Import statements cannot be nested!"

{- Watch/Catch -}
----------------------------------------------------------------
parseTry :: Nesting -> Parser Block
parseTry nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowTry
    let nest = max nesting Nested
    return $ Lexer.IndentMany Nothing (return . Stack.fromList) (statements nest)

parseCatch :: Nesting -> Parser CatchBlock
parseCatch nesting = lexeme . Lexer.indentBlock whitespaceLn $ do
    (void . tryKeyword) meowCatch
    expr <- Mega.optional $ (lexeme . parens) parseExpr
    let makeBody = return . (expr,) . Stack.fromList
    let nest = max nesting Nested
    return $ Lexer.IndentMany Nothing makeBody (statements nest)

manyCatch :: Nesting -> Block -> Parser Statement
manyCatch nesting tryblock = do
    catchblock <- parseCatch nesting
    let expr = StmtTryCatch tryblock catchblock
    manyCatch nesting (Stack.singleton expr) <|> (expr <$ parseEnd)

parseTryCatch :: Nesting -> Parser Statement
parseTryCatch nesting = lexeme $ do
    tryBlock <- parseTry nesting
    Mega.try (manyCatch nesting tryBlock) <|> return (StmtTryCatch tryBlock (Nothing, Stack.empty))


----------------------------------------------------------------
{- Syntatic Sugar -}
----------------------------------------------------------------

{- When -}
-- Syntax is: when <expr> do <expr>
-- Parsed into a simple 'if' statement.
parseWhen :: Nesting -> Parser Statement
parseWhen _ = lexeme $ do
    let (start, end) = meowWhen
    (void . tryKeyword) start
    condition <- parensExpression
    (void . keyword) end
    body <- Stack.singleton . StmtExpr <$> parseExpr
    return (StmtIfElse condition body Stack.empty)


----------------------------------------------------------------
{- Utils -}
----------------------------------------------------------------
loopKeywordOutsideLoop :: Text -> String
loopKeywordOutsideLoop k = concat ["Cannot have '", Text.unpack k, "' statement outside of a loop!"]
