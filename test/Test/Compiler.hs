{-# LANGUAGE OverloadedStrings #-}

module Test.Compiler 
( ParseException(..)
, compileFile
, compileTests
) where

import Data.Text (Text)
import Test.Utils (readText)
import Mewlix.Compiler (compileJS, TranspilerContext(..), emptyContext)
import Mewlix.Packager (ProjectMode(..), addLibraries)
import System.IO (stderr, hPutStrLn)
import Control.Exception (throwIO, Exception(..))
import Conduit
    ( (.|)
    , runConduitRes
    , sourceDirectoryDeep
    , filterC
    , sinkList
    )
import System.FilePath (isExtensionOf)
import Control.Monad ((>=>))
import qualified Data.Text.IO as TextIO

newtype ParseException = ParseException String
    deriving (Show)

instance Exception ParseException where

compileFile :: FilePath -> IO Text
compileFile path = do
    contents <- either (throwIO . ParseException) return =<< readText path

    let libs = addLibraries Library mempty
    let context = emptyContext { imports = libs, pretty = True }

    let handleError :: String -> IO a
        handleError e = do
            hPutStrLn stderr e
            throwIO . ParseException . concat $
                [ "Couldn't parse file '", path, "'!" ]

    putStrLn ("\nCompiling: " ++ show path)
    either handleError return (compileJS context path contents)

findTests :: IO [FilePath]
findTests = runConduitRes
     $ sourceDirectoryDeep True "./test/YarnBalls/"
    .| filterC (isExtensionOf ".mews")
    .| sinkList

compileTests :: IO ()
compileTests = do
    tests <- findTests
    mapM_ (compileFile >=> TextIO.putStrLn) tests
