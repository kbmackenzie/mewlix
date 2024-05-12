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
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Compiler (TranspilerContext, CompilerFunc, CompilerOutput)
import Mewlix.Utils.FileIO (readFileT, appendFileT)
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)

runCompiler :: CompilerFunc -> TranspilerContext -> FilePath -> ProjectMaker CompilerOutput
runCompiler compile context path = readFileT path >>= \case
    (Left err)       -> throwError . concat $ [ "Couldn't read file ", show path, ": ", show err ]
    (Right contents) -> case compile context path contents of
        (Left err)       -> throwError . concat $ [ "Mewlix syntax error in file ", show path, ":\n", err ]
        (Right yarnball) -> return yarnball

-- Compile a Mewlix module and write the output to a new file in the project folder.
-- The function returns the path to the new file.
writeModule :: TranspilerContext -> FilePath -> ProjectMaker ()
writeModule context inputPath = do
    let prepareDirectory :: FilePath -> ProjectMaker ()
        prepareDirectory = liftIO . createDirectoryIfMissing True . takeDirectory

    let outputPath = outputFolder </> "yarnball.js"
    prepareDirectory outputPath

    compiler <- asks projectCompiler
    yarnball <- runCompiler compiler context inputPath

    appendFileT outputPath yarnball
