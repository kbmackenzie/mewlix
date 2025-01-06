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
    , BuildFlags(..)
    , MewlixAction(..)
    , MewlixCommand(..)
    , RunOptions(..)
    , getCommand
    )
import Lens.Micro.Platform ((%~), set)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

run :: IO ()
run = getCommand >>= runCommand

transform :: Maybe a -> (a -> ProjectTransform) -> ProjectTransform
transform Nothing  _  = id
transform (Just a) f  = f a

fromBool :: Bool -> ProjectFlag -> Maybe ProjectFlag
fromBool bool flag = if bool then Just flag else Nothing

projectOptions :: ProjectOptions -> [ProjectTransform]
projectOptions ProjectOptions
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

buildFlags :: BuildFlags -> ProjectTransform
buildFlags BuildFlags
    { prettyFlag   = pretty
    , releaseFlag  = release
    , noStdFlag    = noStd
    , noReadMeFlag = noReadMe } = do
        let flagList = catMaybes
                [ fromBool pretty   Pretty
                , fromBool release  Release
                , fromBool noStd    NoStd
                , fromBool noReadMe NoReadMe ]
        projectFlagsL %~ mappend (Set.fromList flagList)

setQuiet :: Bool -> ProjectTransform
setQuiet quiet = if quiet
    then projectFlagsL %~ Set.insert Quiet
    else id

runCommand :: MewlixCommand-> IO ()
runCommand (MewlixCommand action quiet standalone configPath) = case action of
    (BuildAction options flags) -> do
        let transforms = setQuiet quiet : buildFlags flags : projectOptions options
        make (not standalone) configPath transforms Build

    (RunAction options flags runOpts) -> do
        let runTrans = fromRunOptions runOpts
        let transforms = setQuiet quiet : runTrans : buildFlags flags : projectOptions options
        make (not standalone) configPath transforms Run

    (PackageAction options flags) -> do
        let transforms = setQuiet quiet : buildFlags flags : projectOptions options
        make (not standalone) configPath transforms Package

    (NewAction name mode) -> do
        let transforms =
                [ setQuiet quiet
                , transform name (set projectNameL . Text.pack)
                , transform mode (set projectModeL)             ]
        make False configPath transforms Create

    CleanAction -> do
        let transforms = [setQuiet quiet]
        make (not standalone) configPath transforms Clean

