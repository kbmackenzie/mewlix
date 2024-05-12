{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Compiler 
( ParseException(..)
, compileFile
) where

import Data.Text (Text)
import Mewlix.Compiler.Run (compileJS)
import Mewlix.Compiler.Transpiler (TranspilerContext(..), emptyContext)
import Mewlix.Project.Maker (Language(..))
import Mewlix.Project.Data.Types (ProjectMode(..))
import Mewlix.Project.Modules.StandardLibrary (addLibraries)
import Mewlix.Utils.FileIO (readText)
import System.IO (stderr, hPutStrLn)
import Control.Exception (throwIO, Exception(..))

newtype ParseException = ParseException String
    deriving (Show)

instance Exception ParseException where

compileFile :: FilePath -> IO Text
compileFile path = do
    contents <- readText path >>= \case
        (Left e)    -> throwIO e
        (Right a)   -> return a

    let libs = addLibraries JavaScript Library mempty
    let context = emptyContext { imports = libs, pretty = True }

    case compileJS context path contents of
        (Left e)    -> do
            hPutStrLn stderr e
            (throwIO . ParseException . concat)
                    [ "Couldn't parse file ", show path, "!" ]
        (Right a)   -> return a
