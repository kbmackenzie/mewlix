{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Mewlix.REPL.Core
( REPLData(..)
, REPL(..)
, Line(..)
, LineCommand(..)
, Action(..)
, initREPL
-- Lenses:
, replStateL
, shouldQuitL
-- Re-exports:
, get
, gets
, put
, modify
) where

import Mewlix.Abstract.Meow
import Mewlix.Abstract.State
import Mewlix.Libraries.Base (baseLibrary)
import Mewlix.Interpreter.API (initMeta, initState)
import qualified Mewlix.Data.Stack as Stack
import Mewlix.Parser.AST (LiftedExpr)
import Control.Monad.State (StateT, MonadState, get, gets, put, modify)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Lens.Micro.Platform (makeLensesFor)

data REPLData = REPLData
    { replState  :: MewlixState
    , shouldQuit :: Bool        }

newtype REPL a = REPL { runRepl :: StateT REPLData IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState REPLData )

data Line = Meta LineCommand | Expression LiftedExpr

data LineCommand = LineCommand
    { commandAction :: Action
    , commandArgs   :: [Text]  }

data Action =
      Help
    | Load
    | Quit
    | Inspect
    | Ask
    | Clear
    deriving (Eq, Ord, Enum, Bounded, Show)

initREPL :: (MonadIO m) => m MewlixState
initREPL = do
    meta <- initMeta
    let info = ModuleInfo {
        modulePath = ".",       -- REPL works in the current directory.
        moduleIsMain = True     -- REPL is always the 'main' module.
    }
    baseLib <- baseLibrary
    let libs = Libraries { getLibs = Stack.singleton baseLib }

    initState meta libs info

$(makeLensesFor
    [ ("replState"  , "replStateL"  )
    , ("shouldQuit" , "shouldQuitL" ) ] ''REPLData)
