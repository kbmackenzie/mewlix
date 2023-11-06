{-# LANGUAGE BangPatterns #-}

module Mewlix.Interpreter.API
( runFile
, runMeow
, ReturnValue(..)
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Data.Key (Key)
import Mewlix.Abstract.State
import Mewlix.Interpreter.Import
import Mewlix.Interpreter.Module
import Mewlix.Interpreter.Interpret (ReturnValue(..), statement)
import Mewlix.Libraries.Base (baseLibrary)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Mewlix.Data.Stack as Stack
import Mewlix.Parser.AST (isImport, fromImport)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Lens.Micro.Platform ((.~))
import Control.Monad (void)

---------------------------------------------------------------------------------
interpret :: EvaluatorState MeowPrim -> Evaluator a -> IO (Either CatException a)
interpret state = runExceptT . flip runReaderT state . runEvaluator
---------------------------------------------------------------------------------

{- Initialization: -}
---------------------------------------------------------------------------------------------------

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
        moduleSocket    = Nothing,
        moduleArgs      = Stack.empty
    }

initState :: (MonadIO m) => EvaluatorMeta -> Libraries MeowPrim -> ModuleInfo -> m (EvaluatorState MeowPrim)
initState meta libs info = do
    let !envLib = joinLibraries libs
    !environment <- newRef envLib
    return EvaluatorState {
        evaluatorEnv    = environment,
        moduleInfo      = info,
        evaluatorMeta   = meta,
        evaluatorLibs   = libs
    }

{- Main: -}
---------------------------------------------------------------------------------------------------
runFile :: FilePath -> Bool -> [MetaTransform] -> Libraries MeowPrim -> IO (Either CatException ReturnValue)
runFile path isMain transforms libs = do
    meta <- (\m -> foldr ($) m transforms) <$> initMeta
    let info = ModuleInfo {
        modulePath   = path,
        moduleIsMain = isMain
    }
    state <- initState meta libs info

    let run :: Evaluator ReturnValue
        run = do
            (resolvedPath, Module block) <- readModule path True
            local (moduleInfoL.modulePathL .~ resolvedPath) $ do
                let !(imports, rest) = Stack.partition isImport block
                mapM_ (uncurry runAsImport . fromImport) imports
                statement rest

    interpret state run


{- Imports: -}
---------------------------------------------------------------------------------------------------
runAsImport :: FilePath -> Maybe Key -> Evaluator ()
runAsImport path qualified = do
    (resolvedPath, Module block) <- readModule path False
    let info = ModuleInfo {
        modulePath = resolvedPath,
        moduleIsMain = False
    }
    libs     <- asks evaluatorLibs
    cleanEnv <- newRef (joinLibraries libs)
    let stateTransform = (evaluatorEnvL .~ cleanEnv) . (moduleInfoL .~ info)

    importEnv <- local stateTransform $ do
        let !(imports, rest) = Stack.partition isImport block
        mapM_ (uncurry runAsImport . fromImport) imports
        void (statement rest)
        asks evaluatorEnv

    addImport qualified =<< readRef importEnv


{- Presets: -}
---------------------------------------------------------------------------------------------------
runMeow :: FilePath -> IO (Either CatException ReturnValue)
runMeow path = do
    baseLib <- baseLibrary
    let libs = Libraries { getLibs = Stack.singleton baseLib }
    runFile path True [] libs
