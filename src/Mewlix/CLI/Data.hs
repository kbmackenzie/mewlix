{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Mewlix.CLI.Data
( MewlixOptions(..)
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

data MewlixOptions =
      Clean
    | Build { files     :: [FilePath] }
    deriving (Eq, Show, Data, Typeable)

clean :: MewlixOptions
clean = Clean &= help "Clean any project in the current directory."

build :: MewlixOptions
build = Build { files = def &= args }
    &= help "Build a project"
    &= auto

mode :: Mode (CmdArgs MewlixOptions)
mode = cmdArgsMode $ modes [clean, build]
    &= help "Mewlix compiler"
    &= program "mewlix"
    &= summary "mewlix 1.0.0\nMewlix compiler"

runCLI :: IO MewlixOptions
runCLI = cmdArgsRun mode
