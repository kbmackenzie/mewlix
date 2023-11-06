{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Parser.Expr
( exprL
, exprR
, declaration
, liftedExpr
) where

import Mewlix.Parser.AST
import Mewlix.Parser.Prim
import Mewlix.Parser.Utils
import qualified Mewlix.Data.Stack as Stack
import Mewlix.Parser.Keywords
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

termL :: Parser Expr
termL = ExprKey <$> parseKey

termR :: Parser Expr
termR = Mega.choice
    [ parens exprR
    , parseBox
    , parseList
    , ExprPrim <$> parsePrim
    , ExprKey  <$> parseName ]

exprL :: Parser Expr
exprL = makeExprParser termL operatorsL

exprR :: Parser Expr
exprR = makeExprParser termR operatorsR


{- Data -}
------------------------------------------------------------------------------------
parseList :: Parser Expr
parseList = ExprList <$> bracketList exprR <?> "list"

parseBox :: Parser Expr
parseBox = do
    let parsePair :: Parser (Key, Expr)
        parsePair = do
            key <- parseName
            symbol ':'
            value <- exprR
            return (key, value)

    keyword meowBox
    ExprBox <$> bracketList parsePair <?> "box"


{- Operators -}
------------------------------------------------------------------------------------
dotOp :: Parser (Expr -> Expr)
dotOp = do
    Mega.try $ do
        symbol '.'
        Mega.notFollowedBy (symbol '.')
    flip ExprDotOp <$> termL

boxOp :: Parser (Expr -> Expr)
boxOp = ExprBoxAccess <$> brackets exprR

call :: Parser (Expr -> Expr)
call = do
    args <- parensList exprR
    let argCount = Stack.length args
    return (ExprCall args argCount)

postfixes :: Parser (Expr -> Expr)
postfixes = foldr1 (flip (.)) <$> Mega.some (Mega.choice [ dotOp, boxOp, call ])

lambda :: Parser (Expr -> Expr)
lambda = do
    keyword meowLambda
    args <- parensList parseKey
    keyword "=>"
    return (ExprLambda args)


{- Operator Tables -}
------------------------------------------------------------------------------------
type OperatorTable = [[Operator Parser Expr]]

operatorsL :: OperatorTable
operatorsL = [[ Postfix postfixes ]]

operatorsR :: OperatorTable
operatorsR =
    [
        [ Postfix postfixes                                                ]
    ,   [ Prefix  (ExprPaw                      <$ keyword meowPaw    )
        , Prefix  (ExprClaw                     <$ keyword meowClaw   )    ]
    ,   [ Postfix (ExprUnop UnopLen             <$ keyword "?!"       )    ] 
    ,   [ Prefix  (ExprUnop UnopListPeek        <$ keyword meowPeek   )
        , InfixL  (ExprPush                     <$ keyword meowPush   )
        , Prefix  (ExprPop                      <$ keyword meowKnock  )    ]
    ,   [ InfixL  (ExprBinop BinopConcat        <$ keyword ".."       )    ]
    ,   [ InfixL  (ExprBinop BinopPow           <$ symbol '^'         )    ]
    ,   [ Prefix  (ExprUnop UnopNegate          <$ symbol '-'         )
        , Prefix  (ExprUnop UnopNot             <$ keyword meowNot    )    ]
    ,   [ InfixL  (ExprBinop BinopMul           <$ symbol '*'         )
        , InfixL  (ExprBinop BinopDiv           <$ symbol '/'         )
        , InfixL  (ExprBinop BinopMod           <$ symbol '%'         )    ]
    ,   [ InfixL  (ExprBinop BinopAdd           <$ symbol '+'         )
        , InfixL  (ExprBinop BinopSub           <$ symbol '-'         )    ]
    ,   [ InfixL  (ExprBinop BinopCompareLEQ    <$ keyword "<="       )
        , InfixL  (ExprBinop BinopCompareGEQ    <$ keyword ">="       )
        , InfixL  (ExprBinop BinopCompareLess   <$ symbol '<'         )
        , InfixL  (ExprBinop BinopCompareGreat  <$ symbol '>'         )    ]
    ,   [ InfixL  (ExprBinop BinopCompareEq     <$ keyword "=="       )
        , InfixL  (ExprBinop BinopCompareNotEq  <$ keyword "!="       )    ]
    ,   [ InfixL  (ExprAnd                      <$ keyword meowAnd    )    ]
    ,   [ InfixL  (ExprOr                       <$ keyword meowOr     )    ]
    ,   [ Prefix  lambda                                                   ]
    ,   [ TernR   ((ExprTernary <$ symbol ':')  <$ symbol '?'         )    ]
    ,   [ InfixR  (ExprAssign                   <$ symbol '='         )    ]
    ]

{- Declaration -}
------------------------------------------------------------------------------------
declaration :: Parser (Key, Expr)
declaration = do
    let getValue :: Parser Expr
        getValue = Mega.choice
            [ symbol '=' >> exprR
            , return (ExprPrim PrimNil) ]

    keyword meowLocal
    name   <- parseName
    rvalue <- getValue
    return (name, rvalue)

{- Lifted Expr -}
------------------------------------------------------------------------------------
liftedExpr :: Parser LiftedExpr
liftedExpr = Mega.choice
    [ uncurry LiftDecl <$> declaration
    , LiftExpr         <$> exprR        ]
