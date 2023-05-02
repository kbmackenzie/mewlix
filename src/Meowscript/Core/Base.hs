{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE LambdaCase #-}

module Meowscript.Core.Base
( baseLibrary
) where

import Meowscript.Core.AST
import Meowscript.Core.Environment
import Meowscript.Core.Exceptions
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as Read
import Control.Monad.Except(throwError)
import Control.Monad.State(liftIO)
import Data.Functor((<&>))

baseLibrary :: EnvStack
baseLibrary = ((: []) . Map.fromList)
    [ ("meow"    , MeowIFunc  ["x"] meow      )
    , ("listen"  , MeowIFunc  [   ] listen    )
    , ("reverse" , MeowIFunc  ["x"] reverseFn )
    , ("sort"    , MeowIFunc  ["x"] sortFn    )
    , ("int"     , MeowIFunc  ["x"] toInt     )
    , ("float"   , MeowIFunc  ["x"] toDouble  )
    , ("string"  , MeowIFunc  ["x"] toString  )]

{- IO -} 
----------------------------------------------------------
meow :: Evaluator Prim
meow = lookUpVar "x" >>= (liftIO . TextIO.putStr . asString) >> return MeowLonely

listen :: Evaluator Prim
listen = liftIO TextIO.getLine <&> MeowString

{- Lists/Strings -}
----------------------------------------------------------
reverseFn :: Evaluator Prim
reverseFn = lookUpVar "x" >>= \case
    (MeowList x) -> (return . MeowList . reverse) x
    (MeowString x) -> (return . MeowString . Text.reverse) x
    x -> throwError (badArgs "reverse" [x])

sortFn :: Evaluator Prim
sortFn = lookUpVar "x" >>= \case
    (MeowList x) -> (return . MeowList . List.sort) x
    x -> throwError (badArgs "sort" [x])


{- Conversion -}
----------------------------------------------------------

toString :: Evaluator Prim
toString = lookUpVar "x" <&> (MeowString . asString)

toInt :: Evaluator Prim
toInt = lookUpVar "x" >>= \case
    a@(MeowInt _) -> return a
    (MeowDouble x) -> (return . MeowInt . floor) x
    (MeowString x) -> MeowInt <$> readInt x
    (MeowBool x) -> (return . MeowInt . fromEnum) x
    x -> throwError (badArgs "int" [x])

readInt :: Text.Text -> Evaluator Int
readInt txt = case Read.decimal txt of
    (Left er) -> throwError (badValue "int" (Text.pack er) [MeowString txt])
    (Right x) -> (return . fst) x

toDouble :: Evaluator Prim
toDouble = lookUpVar "x" >>= \case
    a@(MeowDouble _) -> return a
    (MeowInt x) -> (return . MeowDouble . fromIntegral) x
    (MeowString x) -> MeowDouble <$> readDouble x
    (MeowBool x) -> (return . MeowDouble . fromIntegral . fromEnum) x
    x -> throwError (badArgs "float" [x])

readDouble :: Text.Text -> Evaluator Double
readDouble txt = case Read.double txt of
    (Left er) -> throwError (badValue "float" (Text.pack er) [MeowString txt])
    (Right x) -> (return . fst) x

