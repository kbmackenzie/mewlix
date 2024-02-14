{-# LANGUAGE LambdaCase #-}

module Mewlix.CLI.Main
( run
) where

import Mewlix.Project
    ( Language(..)
    , Action(..)
    , make
    , ProjectTransform
    -- Lenses:
    , projectSourceFilesL
    , projectNameL
    , projectModeL
    , projectEntrypointL
    , projectPortL
    )
import Mewlix.CLI.Options
    ( ProjectOptions(..)
    , MewlixOptions(..)
    , getOptions
    )
import Lens.Micro.Platform ((%~), set)
import qualified Data.Text as Text

run :: IO ()
run = getOptions >>= runOption Javascript

fromOptions :: ProjectOptions -> [ProjectTransform]
fromOptions ProjectOptions { filesOpt = files, nameOpt = name, entryOpt = entry, modeOpt = mode } =

    let transform :: Maybe a -> (a -> ProjectTransform) -> ProjectTransform
        transform Nothing  _  = id
        transform (Just a) f  = f a

    in  [ projectSourceFilesL %~ (files ++)
        , transform name  (set projectNameL . Text.pack)
        , transform entry (set projectEntrypointL . Text.pack)
        , transform mode  (set projectModeL) ]

runOption :: Language -> MewlixOptions -> IO ()
runOption language = \case
    (BuildOpt options) -> do
        let transforms = fromOptions options
        make transforms language Build

    (RunOpt options port) -> do
        let portFunc = maybe id (set projectPortL) port
        let transforms = portFunc : fromOptions options
        make transforms language Run

    (PackageOpt options) -> do
        let transforms = fromOptions options
        make transforms language Package

    CleanOpt -> make mempty language Clean
