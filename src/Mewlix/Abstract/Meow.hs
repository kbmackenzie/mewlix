{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Mewlix.Abstract.Meow
( Evaluator(..)
, MeowPrim(..)
, PrimRef
, IFunc
, BoxedString(..)
, BoxedStack(..)
, MeowPairs(..)
, MeowFunction(..)
, MeowIFunction(..)
, MeowMethod(..)
, FuncMap
, MeowClass(..)
, CatBox(..)
, BoxMap
, CatException(..) 
, MeowException(..)
-- Re-exports:
, liftIO
, ask
, asks
, local
, throwError
, catchError
, safeIO
, (>>|)
-- Utils:
, packBox
, unpackBox
, catBoxGet
, catBoxPut
, lookUpRef
, lookUp
, contextWrite
, contextDefine
, contextPush
, runLocal
, runClosure
, contextHas
, showException
, toBoxedString
, toBoxedStack
, listToBoxedStack
, boxedStackPop
, boxedStackPush
, boxedStringTail
, liftToMeow
) where

-- Meow:
import Mewlix.Data.Ref
import Mewlix.Data.ToString
import Mewlix.Abstract.State
import Mewlix.Parser.AST
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Mewlix.IO.SafeIO (SafeIO(..))
import Mewlix.Data.Stack (Stack(..))
import qualified Mewlix.Data.Stack as Stack
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Data.Functor ((<&>))
import Control.Monad.Except (ExceptT, throwError, catchError, MonadError)
import Control.Monad.Reader (ReaderT, ask, asks, local, MonadReader)
import Control.Monad.IO.Class (MonadIO(..))
import Lens.Micro.Platform ((.~))
import Mewlix.Utils.Show

------------------------------------------------------------------------------------
{- Evaluator -}
------------------------------------------------------------------------------------
newtype Evaluator a = Evaluator
    { runEvaluator :: ReaderT (EvaluatorState MeowPrim) (ExceptT CatException IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader (EvaluatorState MeowPrim)
             , MonadError CatException
             )

instance SafeIO Evaluator where
    safeIO m = liftIO m >>= \case
        (Left e)  -> throwError CatException {
            exceptionType = MeowBadIO,
            exceptionMessage = e
        }
        (Right a) -> return a

------------------------------------------------------------------------------------
{- Mewlix Primitives -}
------------------------------------------------------------------------------------
type PrimRef = Ref MeowPrim
type IFunc = Evaluator MeowPrim
type Closure = Ref (Environment MeowPrim)

data MeowPrim =
      MeowInt Int
    | MeowFloat Double
    | MeowString BoxedString
    | MeowBool Bool
    | MeowStack BoxedStack
    | MeowBox CatBox
    | MeowFunc MeowFunction
    | MeowIFunc MeowIFunction
    | MeowMFunc MeowMethod
    | MeowClassDef MeowClass
    | MeowNil

------------------------------------------------------------------------------------
{- Boxed Types -}
------------------------------------------------------------------------------------
data BoxedString = BoxedString
    { unboxStr  :: Text
    , strLen    :: Int       }
    deriving (Show)

instance Eq BoxedString where
    a == b = (strLen a == strLen b) && (unboxStr a == unboxStr b)

instance Ord BoxedString where
    a `compare` b =
        (strLen a `compare` strLen b)
        `mappend` (unboxStr a `compare` unboxStr b)

instance Semigroup BoxedString where
    a <> b = BoxedString (unboxStr a <> unboxStr b) (strLen a + strLen b)

instance ToString BoxedString where
    toString = toString . unboxStr

data BoxedStack = BoxedStack
    { unboxStack :: Stack MeowPrim
    , stackLen   :: Int            }

instance Semigroup BoxedStack where
    a <> b = BoxedStack (unboxStack a <> unboxStack b) (stackLen a + stackLen b)

-- An utility newtype that'll be useful when constructing new boxes later!
newtype MeowPairs = MeowPairs { getPairs :: [(Key, MeowPrim)] }


------------------------------------------------------------------------------------
{- Functions -}
------------------------------------------------------------------------------------
data MeowFunction = MeowFunction
    { funcName      :: Key
    , funcArity     :: Int
    , funcParams    :: Stack Text
    , funcBody      :: Stack Statement
    , funcClosure   :: Closure          }

data MeowIFunction = MeowIFunction
    { ifuncName     :: Key
    , ifuncArity    :: Int
    , ifuncParams   :: Stack Text
    , ifunc         :: IFunc            }

data MeowMethod = MeowMethod
    { methodOwner   :: CatBox       
    , methodFunc    :: MeowFunction     }


------------------------------------------------------------------------------------
{- Mewlix Boxes -}
------------------------------------------------------------------------------------
newtype CatBox = CatBox { getBox :: Ref BoxMap }

-- Type alias for convenience:
type BoxMap = HashMap.HashMap Key PrimRef

packBox :: (MonadIO m) => HashMap Key MeowPrim -> m CatBox
packBox xs = do
    items <- mapM newRef xs
    (fmap CatBox . newRef) items

unpackBox :: (MonadIO m) => CatBox -> m (HashMap Key MeowPrim)
unpackBox box = readRef (getBox box) >>= mapM readRef

catBoxGet :: (MonadIO m) => Key -> CatBox -> m (Maybe PrimRef)
catBoxGet key box = do
    boxMap <- (readRef . getBox) box
    return $ HashMap.lookup key boxMap

catBoxPut :: (MonadIO m) => Key -> MeowPrim -> CatBox -> m ()
catBoxPut key value box = do
    let !boxRef = getBox box
    !valueRef <- newRef value
    modifyRef (HashMap.insert key valueRef) boxRef


------------------------------------------------------------------------------------
{- Mewlix Classes -}
------------------------------------------------------------------------------------
type FuncMap = HashMap Key MeowFunction

data MeowClass = MeowClass
    { className     :: Key
    , classFuncs    :: FuncMap
    , classParent   :: Maybe MeowClass
    , classConstr   :: Maybe MeowFunction }


------------------------------------------------------------------------------------
{- Cat Exception -}
------------------------------------------------------------------------------------
data CatException = CatException
    { exceptionType     :: MeowException
    , exceptionMessage  :: Text     }

data MeowException =
      MeowUnexpected
    | MeowArity
    | MeowTypeMismatch
    | MeowUnboundKey
    | MeowDivByZero
    | MeowNotBox
    | MeowNotProperty
    | MeowNotIdentifier
    | MeowCatOnComputer
    | MeowBadIO
    -- Uncatchable:
    | MeowBadImport
    deriving (Eq, Ord, Enum, Bounded)

instance Show MeowException where
    show x = (++ "Exception") $ case x of
        MeowUnexpected      -> "Unexpected"
        MeowArity           -> "Arity"
        MeowTypeMismatch    -> "Type"
        MeowUnboundKey      -> "Unbound"
        MeowDivByZero       -> "DivisionByZero"
        MeowNotBox          -> "InvalidBox"
        MeowNotProperty     -> "UnboundProperty"
        MeowNotIdentifier   -> "InvalidKey"
        MeowCatOnComputer   -> "CatOnComputer"
        MeowBadIO           -> "IO"
        MeowBadImport       -> "Import"

------------------------------------------------------------------------------------
{- Utils -}
------------------------------------------------------------------------------------
--- Environment ---
lookUpRef :: Key -> Evaluator PrimRef
{-# INLINE lookUpRef #-}
lookUpRef key = do 
    env <- asks evaluatorEnv >>= readRef
    let lookupf = HashMap.lookup key . getEnv
    case lookupf env of
        Nothing    -> throwError CatException {
            exceptionType = MeowUnboundKey,
            exceptionMessage = Text.concat [ "Unbound key: ", key ]
        }
        (Just ref) -> return ref

lookUp :: Key -> Evaluator MeowPrim
{-# INLINE lookUp #-}
lookUp key = lookUpRef key >>= readRef

contextWrite :: Key -> MeowPrim -> Evaluator ()
{-# INLINE contextWrite #-}
contextWrite key value = do
    env <- asks evaluatorEnv >>= readRef
    case HashMap.lookup key (getEnv env) of
        (Just ref) -> writeRef value ref
        Nothing    -> throwError CatException {
            exceptionType = MeowUnboundKey,
            exceptionMessage = Text.concat [ "Unbound key: ", key ]
        }

contextDefine :: Key -> MeowPrim -> Evaluator ()
{-# INLINE contextDefine #-}
contextDefine key value = do
    env      <- asks evaluatorEnv
    valueRef <- newRef value
    let f = Environment . HashMap.insert key valueRef . getEnv
    modifyRef f env

contextPush :: Key -> Ref MeowPrim -> Evaluator ()
{-# INLINE contextPush #-}
contextPush key ref = do
    env <- asks evaluatorEnv
    let f = Environment . HashMap.insert key ref . getEnv 
    modifyRef f env

runLocal :: Evaluator a -> Evaluator a
{-# INLINE runLocal #-}
runLocal action = do
    newEnv <- asks evaluatorEnv >>= copyRef
    local (evaluatorEnvL .~ newEnv) action

runClosure :: Closure -> Evaluator a -> Evaluator a
{-# INLINE runClosure #-}
runClosure closure action = do
    newEnv <- copyRef closure
    local (evaluatorEnvL .~ newEnv) action

contextHas :: Key -> Evaluator Bool
{-# INLINE contextHas #-}
contextHas key = do
    envmap <- asks evaluatorEnv >>= readRef <&> getEnv
    return $ HashMap.member key envmap

--- Exceptions ----
showException :: CatException -> Text
showException e = Text.concat [ "[", (showT . exceptionType) e , "] ", exceptionMessage e ]

-- Boxed --
toBoxedString :: Text -> BoxedString
toBoxedString x = BoxedString { unboxStr = x, strLen = Text.length x }

toBoxedStack :: Stack MeowPrim -> BoxedStack
toBoxedStack xs = BoxedStack { unboxStack = xs, stackLen = Stack.length xs }

listToBoxedStack :: [MeowPrim] -> BoxedStack
listToBoxedStack xs = BoxedStack { unboxStack = Stack.fromList xs, stackLen = length xs }

boxedStackPop :: BoxedStack -> BoxedStack
boxedStackPop (BoxedStack stack n) = case stack of
    Bottom -> error "Mewlix.Abstract.Atom.boxedStackPop: Cannot pop empty stack!"
    (_ ::| xs) -> BoxedStack { unboxStack = xs, stackLen = n - 1 }

boxedStackPush :: MeowPrim -> BoxedStack -> BoxedStack
x `boxedStackPush` BoxedStack xs n  = BoxedStack (x ::| xs) (n + 1)

boxedStringTail :: BoxedString -> BoxedString
boxedStringTail (BoxedString str n) = if Text.null str
    then error "Mewlix.Abstract.Atom.boxedStringTail: Cannot get tail of empty string!"
    else BoxedString { unboxStr = Text.tail str, strLen = n - 1 }

-- Lifting --
liftToMeow :: ParserPrim -> MeowPrim
liftToMeow (PrimInt n) = MeowInt n
liftToMeow (PrimStr s) = (MeowString . toBoxedString) s
liftToMeow (PrimFloat f) = MeowFloat f
liftToMeow (PrimBool b) = MeowBool b
liftToMeow PrimNil = MeowNil


{- Specialization -}
--------------------------------------------------------------------------------
-- MeowPrim refs:
{-# SPECIALIZE newRef     :: MeowPrim -> Evaluator (Ref MeowPrim)                      #-}
{-# SPECIALIZE readRef    :: Ref MeowPrim -> Evaluator MeowPrim                        #-}
{-# SPECIALIZE writeRef   :: MeowPrim -> Ref MeowPrim -> Evaluator ()                  #-}
{-# SPECIALIZE modifyRef  :: (MeowPrim -> MeowPrim) -> Ref MeowPrim -> Evaluator ()    #-}

-- Environment refs:
{-# SPECIALIZE newRef     :: Environment MeowPrim -> Evaluator (Ref (Environment MeowPrim))          #-}
{-# SPECIALIZE readRef    :: Ref (Environment MeowPrim) -> Evaluator (Environment MeowPrim)          #-}
{-# SPECIALIZE writeRef   :: Environment MeowPrim -> Ref (Environment MeowPrim) -> Evaluator ()      #-}
{-# SPECIALIZE modifyRef  :: (Environment MeowPrim -> Environment MeowPrim) -> Ref (Environment MeowPrim) -> Evaluator () #-}
