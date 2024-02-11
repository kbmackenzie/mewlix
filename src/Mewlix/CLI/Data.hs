{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Mewlix.CLI.Data
( MewlixCLI(..)
, runCLI
) where

import System.Console.CmdArgs
    ( CmdArgs
    , Mode
    , (&=)
    , help
    , auto
    , def
    , args
    , program
    , summary
    , modes
    , cmdArgsMode
    , cmdArgsRun
    , Data
    , Typeable
    )

data MewlixCLI =
      Clean
    | Build { files     :: [FilePath] }
    deriving (Eq, Show, Data, Typeable)

clean :: MewlixCLI
clean = Clean &= help "Clean any project in the current directory."

build :: MewlixCLI
build = Build { files = def &= args }
    &= help "Build a project"
    &= auto

mode :: Mode (CmdArgs MewlixCLI)
mode = cmdArgsMode $ modes [clean, build]
    &= help "Mewlix compiler"
    &= program "mewlix"
    &= summary "mewlix 1.0.0\nMewlix compiler"

runCLI :: IO MewlixCLI
runCLI = cmdArgsRun mode
