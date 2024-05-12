{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Packager.Actions.Node
( node
) where

import Mewlix.Packager.Maker (ProjectMaker, throwError)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Data.Types (ProjectData(..))
import Mewlix.Packager.Log (projectLog)
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
