{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Modules.ModuleWriter
( writeModule
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
import System.FilePath ((</>), takeDirectory, isRelative)
import System.Directory (createDirectoryIfMissing, makeRelativeToCurrentDirectory)

compileModule :: CompilerFunc -> TranspilerContext -> FilePath -> ProjectMaker CompilerOutput
compileModule compile context path = readFileT path >>= \case
    (Left err)       -> throwError $ concat [ "Couldn't read file ", show path, ": ", show err ]
    (Right contents) -> case compile context path contents of
        (Left err)       -> throwError $ concat [ "Mewlix syntax error in file ", show path, ":\n", err ]
        (Right yarnball) -> return yarnball

-- Compile a Mewlix module and write the output to a new file in the project folder.
-- The function returns the path to the new file.
writeModule :: TranspilerContext -> FilePath -> ProjectMaker FilePath
writeModule context inputPath = do
    let prepareDirectory :: FilePath -> ProjectMaker ()
        prepareDirectory = liftIO . createDirectoryIfMissing True . takeDirectory

    outputPath <- do
        relative <- liftIO (makeRelativeToCurrentDirectory inputPath)
        if isRelative relative
            then return (moduleFolder </> relative)
            else throwError $ concat
                [ "Source file path cannot be made relative to current directory: "
                , show inputPath
                , "!\nPlease use relative paths without indirections!" ]

    prepareDirectory outputPath
    compiler <- asks projectCompiler
    yarnball <- compileModule compiler context inputPath

    writeFileT outputPath yarnball
    return outputPath
