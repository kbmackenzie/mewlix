{-# LANGUAGE BangPatterns #-}

module Meowscript.Interpreter.API
( runFile
) where

import Meowscript.Abstract.Meow
import Meowscript.Data.Ref
import Meowscript.Abstract.State
import Meowscript.Interpreter.Import
import Meowscript.Interpreter.Module
import Meowscript.Interpreter.Interpret (ReturnValue(..), statement)
import Data.Text (Text)
import Data.Set (Set)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Lens.Micro.Platform ((.~))

---------------------------------------------------------------------------------
interpret :: EvaluatorState MeowPrim -> Evaluator a -> IO (Either CatException a)
interpret state = runExceptT . flip runReaderT state . runEvaluator
---------------------------------------------------------------------------------
type MetaTransform = EvaluatorMeta -> EvaluatorMeta

initMeta :: (MonadIO m) => m EvaluatorMeta
initMeta = do
    cacheMap <- newRef HashMap.empty
    let !cache = ModuleCache cacheMap
    return EvaluatorMeta {
        cachedModules   = cache,
        flagSet         = Set.empty,
        defineMap       = HashMap.empty,
        includePaths    = [],
        moduleSocket    = Nothing
    }

initState :: (MonadIO m) => EvaluatorMeta -> Libraries MeowPrim -> ModuleInfo -> m (EvaluatorState MeowPrim)
initState meta libs info = do
    let !lib = joinLibraries libs
    !env <- newRef lib
    return EvaluatorState {
        evaluatorEnv    = env,
        moduleInfo      = info,
        evaluatorMeta   = meta,
        evaluatorLibs   = libs
    }

runFile :: FilePath -> Bool -> [MetaTransform] -> Libraries MeowPrim -> IO (Either CatException ReturnValue)
runFile path isMain transforms libs = do
    meta <- (\x -> foldr ($) x transforms) <$> initMeta
    let info = ModuleInfo {
        modulePath   = path,
        moduleIsMain = isMain
    }
    state <- initState meta libs info

    let run :: Evaluator ReturnValue
        run = do
            (resolvedPath, Module block) <- readModule path
            local (moduleInfoL.modulePathL .~ resolvedPath) (statement block)
    interpret state run

--runAsImport :: EvalautorState MeowPrim -> 


{- Evaluator Presets -}
---------------------------------------------------------------------------------------------------
runMain :: Module -> Evaluator MeowPrim
runMain (Module block) = do
    undefined
