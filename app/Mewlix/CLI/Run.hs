{-# LANGUAGE LambdaCase #-}

module Mewlix.CLI.Run
( run
) where

import Mewlix.Packager
    ( Action(..)
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
    , projectAssetsL
    )
import Mewlix.CLI.Options
    ( ProjectOptions(..)
    , FlagOptions(..)
    , MewlixOptions(..)
    , RunOptions(..)
    , getOptions
    )
import Lens.Micro.Platform ((%~), set)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

run :: IO ()
run = getOptions >>= runOption

transform :: Maybe a -> (a -> ProjectTransform) -> ProjectTransform
transform Nothing  _  = id
transform (Just a) f  = f a

fromBool :: Bool -> ProjectFlag -> Maybe ProjectFlag
fromBool bool flag = if bool then Just flag else Nothing

fromOptions :: ProjectOptions -> [ProjectTransform]
fromOptions ProjectOptions
    { filesOpt  = files
    , nameOpt   = name
    , entryOpt  = entry
    , modeOpt   = mode
    , assetsOpt = assets } =
        [ projectSourceFilesL %~ (files ++)
        , projectAssetsL %~ (assets ++)
        , transform name  (set projectNameL . Text.pack)
        , transform entry (set projectEntrypointL . Text.pack)
        , transform mode  (set projectModeL)                    ]

fromRunOptions :: RunOptions -> ProjectTransform
fromRunOptions RunOptions
    { runPort      = port
    , runRebuild   = rebuild
    , runNoBrowser = noBrowser } = do
        let setPort  = maybe id (set projectPortL) port
        let flags    = catMaybes
                [ fromBool rebuild   Rebuild
                , fromBool noBrowser NoBrowser ]
        let addFlags = projectFlagsL %~ mappend (Set.fromList flags)
        setPort . addFlags

fromFlags :: FlagOptions -> ProjectTransform
fromFlags FlagOptions
    { quietFlag    = quiet
    , prettyFlag   = pretty
    , noStdFlag    = noStd
    , noReadMeFlag = noReadMe } = do
        let flagList = catMaybes
                [ fromBool quiet    Quiet
                , fromBool pretty   Pretty
                , fromBool noStd    NoStd
                , fromBool noReadMe NoReadMe ]
        projectFlagsL %~ mappend (Set.fromList flagList)

runOption :: MewlixOptions -> IO ()
runOption = \case
    (BuildOpt options flags standalone) -> do
        let transforms = fromFlags flags : fromOptions options
        make (not standalone) transforms Build

    (RunOpt options flags standalone runOpts) -> do
        let runTrans = fromRunOptions runOpts
        let transforms = runTrans : fromFlags flags : fromOptions options
        make (not standalone) transforms Run

    (PackageOpt options flags standalone) -> do
        let transforms = fromFlags flags : fromOptions options
        make (not standalone) transforms Package

    (NewOpt name mode) -> do
        let transforms =
                [ transform name (set projectNameL . Text.pack)
                , transform mode (set projectModeL)             ]
        make False transforms Create

    (CleanOpt standalone) -> do
        make (not standalone) mempty Clean
