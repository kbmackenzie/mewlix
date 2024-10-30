{-# LANGUAGE OverloadedStrings #-}

module Mewlix.Packager.Actions.Node
( runNode
) where

import Mewlix.Packager.Type (Packager, liftIO, throwError)
import Mewlix.Packager.Folder (buildFolder)
import Mewlix.Packager.Config (ProjectConfig(..))
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

runNode :: ProjectConfig -> Packager ()
runNode config = do
    projectLog config "Running project with 'node':"

    let script = "import run from './index.js'; run();"
    exitCode <- liftIO $
        withCurrentDirectory buildFolder (node script)

    case exitCode of
        ExitSuccess     -> return ()
        (ExitFailure n) -> throwError . mconcat $
            [ "Process 'node' exited with code ", show n, "!" ]
