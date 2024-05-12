{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mewlix.Packager.Maker
( PackageContext(..)
, Language(..)
, PackageMaker(..)
, packageMake
, packageMakeJS
, langExtension
, defaultLanguage
, packageContextEmpty
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

data PackageContext = PackageContext
    { projectCompiler  :: CompilerFunc
    , projectLanguage  :: Language     }

-- I might add another language option in the future, which is why this data type exists.
-- If I do, this data type guarantees the process will be much easier.
data Language = JavaScript
    deriving (Eq, Ord, Show, Enum, Bounded)

newtype PackageMaker a = PackageMaker
    { runPackageMaker :: ReaderT PackageContext (ExceptT String IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError String
             , MonadReader PackageContext
             )

packageMake :: PackageContext -> PackageMaker a -> IO (Either String a)
packageMake ctx = runExceptT . flip runReaderT ctx . runPackageMaker

packageMakeJS :: PackageMaker a -> IO (Either String a)
packageMakeJS = packageMake PackageContext
    { projectCompiler  = compileJS
    , projectLanguage  = JavaScript }

langExtension :: Language -> String
langExtension JavaScript = "js"

defaultLanguage :: Language
defaultLanguage = JavaScript

packageContextEmpty :: PackageContext
packageContextEmpty = PackageContext
    { projectCompiler  = \_ _ _ -> Left "No compilter specified!"
    , projectLanguage  = JavaScript }
