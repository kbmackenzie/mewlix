{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Project.ProjectMaker
( ProjectContext(..)
, Language(..)
, ProjectMaker(..)
, projectMake
, projectMakeJS
, langExtension
, projectContextEmpty
-- Re-exports:
, liftIO
, asks
, throwError
, catchError
) where

import Mewlix.Compiler (CompilerFunc, compileJS)
import Control.Monad.Reader (ReaderT, MonadReader, asks, runReaderT)
import Control.Monad.Except (ExceptT, MonadError(throwError, catchError), runExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))

data ProjectContext = ProjectContext
    { projectCompiler  :: CompilerFunc
    , projectLanguage  :: Language    }

-- I might add another language option in the future, which is why this data type exists.
-- If I do, this data type guarantees the process will be much easier.
data Language = Javascript
    deriving (Eq, Ord, Show, Enum, Bounded)

newtype ProjectMaker a = ProjectMaker
    { runProjectMaker :: ReaderT ProjectContext (ExceptT String IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError String
             , MonadReader ProjectContext
             )

projectMake :: ProjectContext -> ProjectMaker a -> IO (Either String a)
projectMake ctx = runExceptT . flip runReaderT ctx . runProjectMaker

projectMakeJS :: ProjectMaker a -> IO (Either String a)
projectMakeJS = projectMake ProjectContext
    { projectCompiler  = compileJS
    , projectLanguage  = Javascript }

langExtension :: Language -> String
langExtension Javascript = "js"

projectContextEmpty :: ProjectContext
projectContextEmpty = ProjectContext
    { projectCompiler  = \_ _ _ -> Left "No compilter specified!"
    , projectLanguage  = Javascript }
