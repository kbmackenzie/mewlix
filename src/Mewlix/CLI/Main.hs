{-# LANGUAGE LambdaCase #-}

module Mewlix.CLI.Main
( run
) where

import Mewlix.Project
    ( Language(..)
    , Action(..)
    , ProjectFlag(..)
    , make
    , ProjectTransform
    -- Lenses:
    , projectSourceFilesL
    , projectNameL
    , projectModeL
    , projectEntrypointL
    , projectPortL
    , projectFlagsL
    )
import Mewlix.CLI.Options
    ( ProjectOptions(..)
    , FlagOptions(..)
    , MewlixOptions(..)
    , getOptions
    )
import Lens.Micro.Platform ((%~), (.~), set)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

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

fromFlags :: FlagOptions -> ProjectTransform
fromFlags FlagOptions { quietFlag = quiet, noStdFlag = noStd, noReadMeFlag = noReadMe } = do
    let fromBool :: Bool -> ProjectFlag -> Maybe ProjectFlag
        fromBool bool flag = if bool then Just flag else Nothing

    let flagList = catMaybes
            [ fromBool quiet    Quiet
            , fromBool noStd    NoStd
            , fromBool noReadMe NoReadMe ]
    projectFlagsL .~ Set.fromList flagList

runOption :: Language -> MewlixOptions -> IO ()
runOption language = \case
    (BuildOpt options flags standalone) -> do
        let transforms = fromFlags flags : fromOptions options
        make (not standalone) transforms language Build

    (RunOpt options flags standalone port) -> do
        let portFunc = maybe id (set projectPortL) port
        let transforms = portFunc : fromFlags flags : fromOptions options
        make (not standalone) transforms language Run

    (PackageOpt options flags standalone) -> do
        let transforms = fromFlags flags : fromOptions options
        make (not standalone) transforms language Package

    CleanOpt -> make False mempty language Clean
