{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Modules.ModuleWriter
( writeModule
, writeModules
) where

import Mewlix.Project.Maker
    ( ProjectContext(..)
    , ProjectMaker
    , asks
    , liftIO
    , throwError
    )
import Mewlix.Project.Folder (moduleFolder)
import Mewlix.Compiler (TranspilerContext, CompilerFunc, CompilerOutput)
import Mewlix.Utils.FileIO (readFileT, writeFileT)
import System.FilePath ((</>), takeDirectory, isAbsolute, dropDrive)
import System.Directory (createDirectoryIfMissing, makeRelativeToCurrentDirectory)

compileModule :: CompilerFunc -> TranspilerContext -> FilePath -> ProjectMaker CompilerOutput
compileModule compile context path = readFileT path >>= \case
    (Left err)       -> throwError ("Couldn't read file:" ++ show err)
    (Right contents) -> case compile context path contents of
        (Left err)       -> throwError ("Mewlix syntax error:\n" ++ err)
        (Right yarnball) -> return yarnball

-- Compile a Mewlix module and write the output to a new file in the project folder.
-- The function returns the path to the new file.
writeModule :: TranspilerContext -> FilePath -> ProjectMaker FilePath
writeModule context inputPath = do
    let prepareDirectory :: FilePath -> ProjectMaker ()
        prepareDirectory = liftIO . createDirectoryIfMissing True . takeDirectory

    outputPath <- liftIO $ do
        relative <- makeRelativeToCurrentDirectory inputPath
        folder   <- moduleFolder
        return $ if isAbsolute relative
            then folder </> dropDrive relative
            else folder </> relative

    prepareDirectory outputPath
    compiler <- asks projectCompiler
    yarnball <- compileModule compiler context inputPath

    writeFileT outputPath yarnball
    return outputPath

writeModules :: TranspilerContext -> [FilePath] -> ProjectMaker [FilePath]
writeModules context = mapM (writeModule context)
