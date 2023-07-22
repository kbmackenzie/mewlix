{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Meowr.RunMeowr
( runMeowr 
) where

import Meowscript.Core.AST
import Meowscript.Meowr.Core
import Meowscript.Meowr.Parser
import Meowscript.Core.MeowState (meowState')
import Meowscript.Core.RunEvaluator (runMeow)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Meowscript.Utils.IO
import Meowscript.REPL.Loop (repl)
import Lens.Micro.Platform (over)
import Meowscript.API.JSON (toJSON)
import Control.Monad ((>=>))

argStr :: IO Text.Text
argStr = Text.intercalate " " . map Text.pack <$> getArgs

getMeowr :: IO (Either Text.Text MeowrAction)
getMeowr = parseMeowed <$> argStr

applyArgs :: [MeowrArg] -> MeowState -> MeowState
applyArgs xs state = foldl (flip meowrAction) state xs

meowrAction :: MeowrArg -> MeowState -> MeowState
meowrAction (MeowrFlag flag) = addFlag flag
meowrAction (MeowrOption key value) = addOption key value
meowrAction (MeowrString arg) = addArg arg

transState :: [MeowrArg] -> MeowState -> MeowState
transState = (over meowArgs reverse .) . applyArgs

makeError :: Text.Text -> Text.Text
makeError = Text.append "Syntax error in command:\n"

type Name = Text.Text
type Action = Name -> [MeowrArg] -> IO ()

meowrActions :: Map.Map Name Action
meowrActions = Map.fromList
    [ ("repl", (const . const) repl )
    , ("run" , meowrMake none       )
    , ("json", meowrMake json       )]

runMeowr :: IO ()
runMeowr = getMeowr >>= \case
    (Left err) -> (printErrLn . makeError) err
    (Right (MeowrAction name args)) -> case name of
        Nothing -> repl -- todo: apply args here too
        (Just n) -> runAction n args

getMeowrStr :: MeowrArg -> Text.Text
getMeowrStr (MeowrString x) = x
getMeowrStr _ = undefined

runAction :: Name -> [MeowrArg] -> IO ()
runAction name args = print (Map.keys meowrActions) >> case Map.lookup name meowrActions of
    Nothing -> meowrMake none name args
    (Just f) -> f newName newArgs
    where (newName, newArgs) = case List.partition isMeowrStr args of
              ([], rest) -> ("", rest)
              (x:xs, rest) -> (getMeowrStr x, xs ++ rest)

none :: Prim -> IO ()
none = (const . return) ()

json :: Prim -> IO ()
json = toJSON >=> printStrLn

meowrMake :: (Prim -> IO ()) -> Name -> [MeowrArg] -> IO ()
meowrMake f name args = do
    state <- meowState' name [] (return Map.empty)
    let meowedState = transState args state
    --(print . _meowArgs) meowedState
    --(print . _meowInclude) meowedState
    runMeow meowedState >>= \case
        (Left exc) -> (printExc . snd) exc
        (Right  prim) -> f prim
