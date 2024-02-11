{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Modules.ModuleWriter
( writeModule
, writeModules
) where

import Mewlix.Project.Make
    ( ProjectContext(..)
    , ProjectMaker
    , asks
    , throwError
    )
import qualified Mewlix.Utils.FileIO as FileIO
import Mewlix.Compiler (TranspilerContext, CompilerFunc, CompilerOutput)
import Mewlix.Project.Modules.ProjectFolder (toOutputPath, preparePath)

compileModule :: CompilerFunc -> TranspilerContext -> FilePath -> ProjectMaker CompilerOutput
compileModule compile context path = FileIO.readFile path >>= \case
    (Left err)       -> throwError ("Couldn't read file:" ++ show err)
    (Right contents) -> case compile context path contents of
        (Left err)       -> throwError ("Mewlix syntax error:\n" ++ err)
        (Right yarnball) -> return yarnball

-- Compile a Mewlix module and write the output to a new file in the project folder.
-- The function returns the path to the new file.
writeModule :: TranspilerContext -> FilePath -> ProjectMaker FilePath
writeModule context inputPath = do
    outputPath <- toOutputPath inputPath
    preparePath outputPath

    compiler <- asks projectCompiler
    yarnball <- compileModule compiler context inputPath

    FileIO.writeFile outputPath yarnball
    return outputPath

writeModules :: TranspilerContext -> [FilePath] -> ProjectMaker [FilePath]
writeModules context = mapM (writeModule context)
