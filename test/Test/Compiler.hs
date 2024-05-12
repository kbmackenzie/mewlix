{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler 
( ParseException(..)
, compileFile
) where

import Data.Text (Text)
import Mewlix.Compiler.Run (compileJS)
import Mewlix.Compiler.Transpiler (TranspilerContext(..), emptyContext)
import Mewlix.Packager.Maker (Language(..))
import Mewlix.Packager.Data.Types (ProjectMode(..))
import Mewlix.Packager.Modules.StandardLibrary (addLibraries)
import Mewlix.Utils.FileIO (readText)
import System.IO (stderr, hPutStrLn)
import Control.Exception (throwIO, Exception(..))

newtype ParseException = ParseException String
    deriving (Show)

instance Exception ParseException where

compileFile :: FilePath -> IO Text
compileFile path = do
    contents <- either throwIO return =<< readText path

    let libs = addLibraries JavaScript Library mempty
    let context = emptyContext { imports = libs, pretty = True }

    let handleError :: String -> IO a
        handleError e = do
            hPutStrLn stderr e
            throwIO . ParseException . concat $
                [ "Couldn't parse file ", show path, "!" ]

    putStrLn ("Compiling: " ++ show path)
    either handleError return (compileJS context path contents)
