{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.IO.ModuleWriter
( writeModule
, writeModules
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
import Mewlix.Project.Make
    ( ProjectContext(..)
    , ProjectMaker
    , liftIO
    , asks
    , throwError
    )
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

-- Compile a Mewlix module and write the output to a new file in the project folder.
-- The function returns the path to the new file.
writeModule :: TranspilerContext -> FilePath -> ProjectMaker FilePath
writeModule context inputPath = do
    outputPath <- toOutputPath inputPath
    preparePath outputPath

    compiler <- asks projectCompiler

    let write :: IO ()
        write = runConduitRes
             $ sourceFile inputPath
            .| decodeUtf8C
            .| compileModule compiler context inputPath
            .| encodeUtf8C
            .| sinkFile outputPath

    let handleException :: MewlixException -> ProjectMaker FilePath
        handleException e = do
            let message = "[Mewlix] Syntax error:\n" ++ unwrapMewlixException e
            throwError message

    liftIO (try write) >>= either handleException (return . const outputPath)

writeModules :: TranspilerContext -> [FilePath] -> ProjectMaker [FilePath]
writeModules context = mapM (writeModule context)
