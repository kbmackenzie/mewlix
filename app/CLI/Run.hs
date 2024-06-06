{-# LANGUAGE LambdaCase #-}

module CLI.Run
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
import CLI.Options
    ( ProjectOptions(..)
    , FlagOptions(..)
    , MewlixOptions(..)
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

fromOptions :: ProjectOptions -> [ProjectTransform]
fromOptions ProjectOptions
    { filesOpt = files
    , nameOpt = name
    , entryOpt = entry
    , modeOpt = mode
    , assetsOpt = assets } =
        [ projectSourceFilesL %~ (files ++)
        , projectAssetsL %~ (assets ++)
        , transform name  (set projectNameL . Text.pack)
        , transform entry (set projectEntrypointL . Text.pack)
        , transform mode  (set projectModeL)                    ]

fromFlags :: FlagOptions -> ProjectTransform
fromFlags FlagOptions
    { quietFlag = quiet
    , prettyFlag = pretty
    , noStdFlag = noStd
    , noReadMeFlag = noReadMe } = do
        let fromBool :: Bool -> ProjectFlag -> Maybe ProjectFlag
            fromBool bool flag = if bool then Just flag else Nothing

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

    (RunOpt options flags standalone port noBrowser) -> do
        let portFunc = maybe id (set projectPortL) port
        let browser = if noBrowser
            then projectFlagsL %~ Set.insert NoBrowser
            else id
        let transforms = browser : portFunc : fromFlags flags : fromOptions options
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
