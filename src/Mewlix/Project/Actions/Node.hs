{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Actions.Node
( node
) where

import Mewlix.Project.Maker (ProjectMaker, throwError)
import Mewlix.Project.Folder (outputFolder)
import Mewlix.Project.Data.Types (ProjectData(..))
import Mewlix.Project.Log (projectLog)
import System.FilePath ((</>))
import System.Process.Typed (runProcess, proc, ExitCode(..))

runNode :: FilePath -> ProjectMaker ()
runNode script = runProcess (proc "node" [script]) >>= \case
    ExitSuccess     -> return ()
    (ExitFailure n) -> throwError . mconcat $
        [ "Process 'node' exited with code ", show n, "!" ]

node :: ProjectData -> ProjectMaker ()
node projectData = do
    projectLog projectData "Running project with 'node':"

    let script = outputFolder </> "index.js"
    runNode script
