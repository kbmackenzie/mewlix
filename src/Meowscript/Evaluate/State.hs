{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module Meowscript.Evaluate.State
( Module(..)
, ModuleCache(..)
, EvaluatorMeta(..)
, ModuleInfo(..)
, EvaluatorState(..)
, MeowFlag(..)
, DefineMap
, FlagSet
, CacheMap
-- Lenses:
, cachedModulesL
, defineMapL
, includePathsL
, flagSetL
, moduleSocketL
, modulePathL
, moduleIsMainL
, evaluatorCtxL
, moduleInfoL
, evaluatorMetaL
, getModuleL
, getCacheL
-- Inititializers:
, initMeta
, emptyMeta
-- Setters:
, addDefine
, addFlag
, addInclude
) where

import Meowscript.Data.Ref
import Meowscript.Parser.AST
import Meowscript.Evaluate.Environment
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro.Platform (makeLensesFor, (^.), (%~), over)
import qualified Data.Set as Set
import Control.Monad.IO.Class (MonadIO(..))

newtype Module = Module { getModule :: Block }
newtype ModuleCache = ModuleCache { getCache :: Ref CacheMap }

data EvaluatorMeta = EvaluatorMeta
    { cachedModules :: ModuleCache
    , defineMap     :: DefineMap
    , includePaths  :: [FilePath]
    , flagSet       :: FlagSet
    , moduleSocket  :: Maybe Int   }

data ModuleInfo = ModuleInfo
    { modulePath    :: FilePath
    , moduleIsMain  :: Bool        }

data EvaluatorState p = EvaluatorState
    { evaluatorCtx  :: Context p
    , moduleInfo    :: ModuleInfo
    , evaluatorMeta :: EvaluatorMeta }

{- Flags -}
-------------------------------------------------------------------------------------
data MeowFlag =
      ImplicitMain
    | FullScreen
    deriving (Eq, Ord, Show, Enum, Bounded)

{- Aliases -}
-------------------------------------------------------------------------------------
type DefineMap = HashMap.HashMap Text.Text Text.Text
type FlagSet   = Set.Set MeowFlag
type CacheMap  = HashMap.HashMap FilePath Module

{- Acessors -}
-------------------------------------------------------------------------------------
$(makeLensesFor
    [ ("cachedModules", "cachedModulesL")
    , ("defineMap"    , "defineMapL"    )
    , ("includePaths" , "includePathsL" )
    , ("flagSet"      , "flagSetL"      )
    , ("moduleSocket" , "moduleSocketL" ) ] ''EvaluatorMeta)

$(makeLensesFor
    [ ("modulePath"   , "modulePathL"   )
    , ("moduleIsMain" , "moduleIsMainL" ) ] ''ModuleInfo)

$(makeLensesFor
    [ ("evaluatorCtx" , "evaluatorCtxL" )
    , ("moduleInfo"   , "moduleInfoL"   )
    , ("evaluatorMeta", "evaluatorMetaL") ] ''EvaluatorState)

$(makeLensesFor
    [ ("getModule"    , "getModuleL"    ) ] ''Module)

$(makeLensesFor
    [ ("getCache"     , "getCacheL"     ) ] ''ModuleCache)

{- Initializers -}
-------------------------------------------------------------------------------------
initMeta :: (MonadIO m) => DefineMap -> [FilePath] -> FlagSet -> m EvaluatorMeta
initMeta defmap include flagset = do
    moduleCache <- ModuleCache <$> newRef HashMap.empty
    return EvaluatorMeta {
        cachedModules   = moduleCache,
        defineMap       = defmap,
        includePaths    = include,
        flagSet         = flagset,
        moduleSocket    = Nothing
    }

emptyMeta :: (MonadIO m) => m EvaluatorMeta
emptyMeta = initMeta HashMap.empty [] Set.empty

{- Setters -}
-------------------------------------------------------------------------------------
addDefine :: Text.Text -> Text.Text -> EvaluatorState p -> EvaluatorState p
addDefine key item = (evaluatorMetaL.defineMapL) %~ HashMap.insert key item

addFlag :: MeowFlag -> EvaluatorState p -> EvaluatorState p
addFlag = over (evaluatorMetaL.flagSetL) . Set.insert

addInclude :: FilePath -> EvaluatorState p -> EvaluatorState p
addInclude = over (evaluatorMetaL.includePathsL) . (:)
