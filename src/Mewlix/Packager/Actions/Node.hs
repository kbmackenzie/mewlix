{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Node
( runNode
) where

import Mewlix.Packager.Type (Packager, liftIO, throwError)
import Mewlix.Packager.Folder (outputFolder)
import Mewlix.Packager.Config.Types (ProjectData(..))
import Mewlix.Packager.Log (projectLog)
import System.Directory (withCurrentDirectory)
import System.Process.Typed
    ( runProcess
    , proc
    , setStdin
    , byteStringInput
    , ExitCode(..)
    )
import Data.ByteString.Lazy (ByteString);

node :: ByteString -> IO ExitCode
node script = do
    let input = byteStringInput script
    let args = ["--input-type=module"]
    runProcess . setStdin input $ proc "node" args

runNode :: ProjectData -> Packager ()
runNode projectData = do
    projectLog projectData "Running project with 'node':"

    let script = "import run from './index.js'; run();"
    exitCode <- liftIO $
        withCurrentDirectory outputFolder (node script)

    case exitCode of
        ExitSuccess     -> return ()
        (ExitFailure n) -> throwError . mconcat $
            [ "Process 'node' exited with code ", show n, "!" ]
