{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Meowscript.Abstract.Meow
( Evaluator(..)
, MeowPrim(..)
, PrimRef
, IFunc
, BoxedString(..)
, BoxedStack(..)
, MeowPairs(..)
, MeowFunction(..)
, MeowIFunction(..)
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
, lookUpRef
, lookUp
, contextWrite
, contextDefine
, contextPush
, runLocal
, runClosure
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
import Meowscript.Data.Key (Key)
import Meowscript.Data.Ref
import Meowscript.Data.ToString
import Meowscript.Abstract.State
import Meowscript.Parser.AST
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Meowscript.IO.SafeIO (SafeIO(..))
import Meowscript.Data.Stack (Stack(..))
import qualified Meowscript.Data.Stack as Stack
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Data.Functor ((<&>))
import Control.Monad.Except (ExceptT, throwError, catchError, MonadError)
import Control.Monad.Reader (ReaderT, ask, asks, local, MonadReader)
import Control.Monad.IO.Class (MonadIO(..))
import Lens.Micro.Platform ((.~))
import Meowscript.Utils.Show

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
{- Meowscript Primitives -}
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
    { funcName      :: Identifier
    , funcArity     :: Int
    , funcParams    :: Stack Text
    , funcBody      :: Stack Statement
    , funcClosure   :: Closure        }

data MeowIFunction = MeowIFunction
    { ifuncName     :: Identifier
    , ifuncArity    :: Int
    , ifuncParams   :: Stack Text
    , ifunc         :: IFunc            }


------------------------------------------------------------------------------------
{- Meowscript Boxes -}
------------------------------------------------------------------------------------
newtype CatBox = CatBox { getBox :: Ref BoxMap }

-- Type alias for convenience:
type BoxMap = HashMap.HashMap Key PrimRef


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
        MeowNotIdentifier   -> "InvalidIdentifier"
        MeowBadIO           -> "IO"
        MeowBadImport       -> "Import"

------------------------------------------------------------------------------------
{- Utils -}
------------------------------------------------------------------------------------
--- Environment ---
lookUpRef :: Key -> Evaluator (Maybe PrimRef)
{-# INLINE lookUpRef #-}
lookUpRef key = asks evaluatorEnv >>= readRef <&> HashMap.lookup key . getEnv

lookUp :: Key -> Evaluator MeowPrim
{-# INLINE lookUp #-}
lookUp key = lookUpRef key >>= \case
    Nothing    -> throwError CatException {
        exceptionType = MeowUnboundKey,
        exceptionMessage = Text.concat [ "Unbound key: ", key ]
    }
    (Just ref) -> readRef ref

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
    Bottom -> error "Meowscript.Abstract.Atom.boxedStackPop: Cannot pop empty stack!"
    (_ ::| xs) -> BoxedStack { unboxStack = xs, stackLen = n - 1 }

boxedStackPush :: MeowPrim -> BoxedStack -> BoxedStack
x `boxedStackPush` BoxedStack xs n  = BoxedStack (x ::| xs) (n + 1)

boxedStringTail :: BoxedString -> BoxedString
boxedStringTail (BoxedString str n) = if Text.null str
    then error "Meowscript.Abstract.Atom.boxedStringTail: Cannot get tail of empty string!"
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
