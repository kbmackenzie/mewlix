{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Test.Compiler 
( ParseException(..)
, compileFile
) where

import Data.Text (Text)
import Mewlix.Compiler.Run
import Mewlix.Compiler.Transpiler (emptyContext)
import Mewlix.Utils.FileIO (readFileT)
import System.IO (stderr, hPutStrLn)
import Control.Exception (throwIO, Exception(..))

newtype ParseException = ParseException String
    deriving (Show)

instance Exception ParseException where

compileFile :: FilePath -> IO Text
compileFile path = do
    contents <- readFileT path >>= \case
        (Left e)    -> throwIO e
        (Right a)   -> return a
    case compileJS emptyContext path contents of
        (Left e)    -> do
            hPutStrLn stderr e
            (throwIO . ParseException . concat)
                    [ "Couldn't parse file ", show path, "!" ]
        (Right a)   -> return a
