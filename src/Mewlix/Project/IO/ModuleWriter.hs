{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.IO.ModuleWriter
( writeModule
) where

import Conduit
    ( runConduitRes
    , (.|)
    , sourceFile
    , sinkFile
    , decodeUtf8C
    , encodeUtf8C
    , ConduitT
    , ResourceT
    , yield
    , await
    )
import Mewlix.Project.Maker (ProjectMaker, liftIO, throwError)
import Mewlix.Compiler (TranspilerContext, CompilerFunc, CompilerOutput)
import Mewlix.Parser (FileContent)
import Mewlix.Project.IO.ProjectFolder (toOutputPath, preparePath)
import Data.Typeable (Typeable)
import Control.Exception (throwIO, try, Exception)

{- MewlixException newtype, for throwing IO exceptions on a syntax error.
 -
 - These exceptions are *always* caught and wrapped in an Either
 - inside the ProjectMaker monad.
 -
 - This exception type exists for convenience and should *never* propagate. -}
newtype MewlixException = MewlixException { unwrapMewlixException :: String }
    deriving (Show, Typeable)

instance Exception MewlixException where

------------------------------------------------------------------------------------

type CompilationConduit = ConduitT FileContent CompilerOutput (ResourceT IO) ()

compileModule :: CompilerFunc -> TranspilerContext -> FilePath -> CompilationConduit
compileModule compile context path = await >>= \case
    Nothing         -> return ()
    (Just contents) -> case compile context path contents of
        (Left err)       -> (liftIO . throwIO) (MewlixException err)
        (Right yarnball) -> yield yarnball

writeModule :: CompilerFunc -> TranspilerContext -> FilePath -> ProjectMaker ()
writeModule compiler context inputPath = do
    outputPath <- liftIO (toOutputPath inputPath)
    liftIO (preparePath outputPath)

    let write :: IO ()
        write = runConduitRes
             $ sourceFile inputPath
            .| decodeUtf8C
            .| compileModule compiler context inputPath
            .| encodeUtf8C
            .| sinkFile outputPath

    let handleException :: MewlixException -> ProjectMaker ()
        handleException e = do
            let message = "[Mewlix] Syntax error:\n" ++ unwrapMewlixException e
            throwError message

    liftIO (try write) >>= either handleException return
