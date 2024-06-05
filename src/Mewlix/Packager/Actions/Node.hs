{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Node
( runNode
) where

import Mewlix.Packager.Maker (PackageMaker, liftIO, throwError)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Data.Types (ProjectData(..))
import Mewlix.Packager.Log (projectLog)
import System.Directory (withCurrentDirectory)
import System.Process.Typed (runProcess, proc, ExitCode(..))

node :: FilePath -> IO ExitCode
node script = runProcess $ proc "node" [script]

runNode :: ProjectData -> PackageMaker ()
runNode projectData = do
    projectLog projectData "Running project with 'node':"

    let script = "./auto.js"
    exitCode <- liftIO $
        withCurrentDirectory outputFolder (node script)

    case exitCode of
        ExitSuccess     -> return ()
        (ExitFailure n) -> throwError . mconcat $
            [ "Process 'node' exited with code ", show n, "!" ]
