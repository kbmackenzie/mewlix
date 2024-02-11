{-# LANGUAGE LambdaCase #-}

module Mewlix.Project.Run
(
) where

import Mewlix.Project.Make
    ( ProjectMaker(..)
    , Language(..)
    , ProjectContext(..)
    , projectMakeJS
    )
import Mewlix.Project.Init (initProject)
import qualified Data.ByteString as ByteString

make :: Language -> ProjectMaker () 
make Javascript = projectMakeJS initProject >>= \case
    (Left err) -> do
