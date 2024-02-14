{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Mewlix.CLI.Options
( MewlixOptions(..)
, runOptions
) where

-- This module should be always imported qualified, as the MewlixOptions data type constructors conflict
-- with the Action constructors from Mewlix.Project.Make.

import Mewlix.Project (ProjectMode(..), defaultMode)
import System.Console.CmdArgs
    ( CmdArgs
    , Mode
    , (&=)
    , help
    , auto
    , def
    , args
    , enum
    , typ
    , typFile
    , program
    , summary
    , modes
    , cmdArgsMode
    , cmdArgsRun
    , Data
    , Typeable
    )

data MewlixOptions =
      Build
        { files :: [FilePath]
        , name  :: FilePath
        , mode  :: ProjectMode }
    | Run
        { files :: [FilePath]
        , name  :: FilePath
        , mode  :: ProjectMode
        , port  :: Int         }
    | Package
        { files :: [FilePath]
        , name  :: FilePath
        , mode  :: ProjectMode }
    | Clean
    deriving (Eq, Show, Data, Typeable)

clean :: MewlixOptions
clean = Clean
    &= help "Clean any project in the current directory."

build :: MewlixOptions
build = Build
        { files = def &= typ "BUILD FILES" &= args
        , name  = def &= typ "STRING"
        , mode  = enum
            [ Console &= help "Build with console frontend"
            , Graphic &= help "Build with graphic backend"
            , Library &= help "Build with no frontend"     ]
        }
    &= help "Build project"
    &= auto

run :: MewlixOptions
run = Run
        { files = def &= typ "FILES TO RUN" &= args
        , name  = def &= typ "STRING"
        , mode  = enum
            [ Console &= help "Run with console frontend"
            , Graphic &= help "Run with graphic frontend"
            , Library &= help "Run with no frontend"     ]
        , port  = 8000 &= typ "INT"
        }
    &= help "Build and run project"

package :: MewlixOptions
package = Package
        { files = def &= typFile &= args
        , name  = def &= typ "STRING"
        , mode  = enum
            [ Console &= help "._Console frontend"
            , Graphic &= help "._Graphic backend"
            , Library &= help "._No frontend"     ]
        }
    &= help "Build and package project into a .zip archive"

options :: Mode (CmdArgs MewlixOptions)
options = cmdArgsMode $ modes [build, run, package, clean]
    &= help "mewlix compiler"
    &= program "mewlix"
    &= summary "mewlix 1.0.0\nMewlix compiler"

runOptions :: IO MewlixOptions
runOptions = cmdArgsRun options
