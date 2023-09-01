{-# LANGUAGE TemplateHaskell #-}

module Meowscript.Evaluate.State
( Module(..)
, ModuleCache(..)
, EvaluatorMeta(..)
, ModuleInfo(..)
, EvaluatorState(..)
, MeowFlags(..)
, DefineMap
, FlagSet
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
) where

import Meowscript.Data.Ref
import Meowscript.Parser.AST
import Meowscript.Evaluate.Environment
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro.Platform (makeLensesFor)
import qualified Data.Set as Set

newtype Module = Module { getModule :: [Statement] }
newtype ModuleCache = ModuleCache { getCache :: Ref (HashMap.HashMap Text.Text Module) }

data EvaluatorMeta p = EvaluatorMeta
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
    , evaluatorMeta :: EvaluatorMeta p }

{- Flags -}
-------------------------------------------------------------------------------------
data MeowFlags =
      ImplicitMain
    | FullScreen
    deriving (Eq, Ord, Show, Enum, Bounded)

{- Aliases -}
-------------------------------------------------------------------------------------
type DefineMap = HashMap.HashMap Text.Text Text.Text
type FlagSet   = Set.Set MeowFlags

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
