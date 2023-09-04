{-# LANGUAGE StrictData #-}

module Meowscript.Abstract.Atom
( MeowAtom(..)
, AtomRef
, IFunc
, BoxedString(..)
, BoxedStack(..)
, MeowPairs(..)
, MeowFunction(..)
, MeowIFunction(..)
, CatBox(..)
, BoxMap
, boxString
, boxStack
, boxList
, stackPop
, stackPush
, strTail
, liftToMeow
) where

import Meowscript.Data.Key (Key)
import Meowscript.Data.Ref
import Meowscript.Data.ToString
import Meowscript.Evaluate.Evaluator
import Meowscript.Evaluate.Environment
import Meowscript.Data.Stack (Stack(..))
import qualified Meowscript.Data.Stack as Stack
import Data.Int (Int32)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Meowscript.Parser.AST

{- Meowscript Primitives -}
--------------------------------------------------------------------------------
type AtomRef = Ref MeowAtom
type IFunc = Evaluator MeowAtom MeowAtom

data MeowAtom =
      MeowInt Int32
    | MeowFloat Double
    | MeowString BoxedString
    | MeowBool Bool
    | MeowStack BoxedStack
    | MeowBox CatBox
    | MeowFunc MeowFunction
    | MeowIFunc MeowIFunction
    | MeowNil

{- Boxed Types -}
--------------------------------------------------------------------------------
data BoxedString = BoxedString
    { unboxStr  :: Text.Text
    , strLen    :: Int       }
    deriving (Show)

instance Eq BoxedString where
    a == b = (strLen a == strLen b) && (unboxStr a == unboxStr b)

instance Ord BoxedString where
    a `compare` b = (strLen a `compare` strLen b) `mappend` (unboxStr a `compare` unboxStr b)

instance Semigroup BoxedString where
    BoxedString s1 l1 <> BoxedString s2 l2 = BoxedString (s1 <> s2) (l1 + l2)

instance ToString BoxedString where
    toString = toString . unboxStr

data BoxedStack = BoxedStack
    { unboxStack :: Stack MeowAtom
    , stackLen   :: Int            }

instance Semigroup BoxedStack where
    BoxedStack u1 l1 <> BoxedStack u2 l2 = BoxedStack (u1 <> u2) (l1 + l2)

-- An utility newtype that'll be useful when constructing new boxes later!
newtype MeowPairs = MeowPairs { getPairs :: [(Key, MeowAtom)] }


{- Functions -}
--------------------------------------------------------------------------------
data MeowFunction = MeowFunction
    { funcName      :: Identifier
    , funcArity     :: Int
    , funcParams    :: Stack Text.Text
    , funcBody      :: Stack Statement
    , funcClosure   :: Context MeowAtom }

data MeowIFunction = MeowIFunction
    { ifuncName     :: Identifier
    , ifuncArity    :: Int
    , ifuncParams   :: Stack Text.Text
    , ifunc         :: IFunc            }


{- Meowscript Boxes -}
--------------------------------------------------------------------------------
newtype CatBox = CatBox { getBox :: Ref BoxMap }

-- Type alias for convenience:
type BoxMap = HashMap.HashMap Key AtomRef


{- Utils -}
--------------------------------------------------------------------------------
boxString :: Text.Text -> BoxedString
boxString x = BoxedString { unboxStr = x, strLen = Text.length x }

boxStack :: Stack MeowAtom -> BoxedStack
boxStack xs = BoxedStack { unboxStack = xs, stackLen = Stack.length xs }

boxList :: [MeowAtom] -> BoxedStack
boxList xs = BoxedStack { unboxStack = Stack.fromList xs, stackLen = length xs }

stackPop :: BoxedStack -> BoxedStack
stackPop (BoxedStack stack n) = case stack of
    Bottom -> error "Meowscript.Abstract.Atom.stackPop: Cannot pop empty stack!"
    (_ ::| xs) -> BoxedStack { unboxStack = xs, stackLen = n - 1 }

stackPush :: MeowAtom -> BoxedStack -> BoxedStack
x `stackPush` BoxedStack xs n  = BoxedStack (x ::| xs) (n + 1)

strTail :: BoxedString -> BoxedString
strTail (BoxedString str n) = if Text.null str
    then error "Meowscript.Abstract.Atom.strTail: Cannot get tail of empty string!"
    else BoxedString { unboxStr = Text.tail str, strLen = n - 1 }

liftToMeow :: ParserPrim -> MeowAtom
liftToMeow (PrimInt n) = (MeowInt . fromIntegral) n
liftToMeow (PrimStr s) = (MeowString . boxString) s
liftToMeow (PrimFloat f) = MeowFloat f
liftToMeow (PrimBool b) = MeowBool b
liftToMeow PrimNil = MeowNil


{- Specialization -}
--------------------------------------------------------------------------------
-- Evaluator refs:
{-# SPECIALIZE newRef     :: MeowAtom -> Evaluator MeowAtom (Ref MeowAtom)                      #-}
{-# SPECIALIZE readRef    :: Ref MeowAtom -> Evaluator MeowAtom MeowAtom                        #-}
{-# SPECIALIZE writeRef   :: MeowAtom -> Ref MeowAtom -> Evaluator MeowAtom ()                  #-}
{-# SPECIALIZE modifyRef  :: (MeowAtom -> MeowAtom) -> Ref MeowAtom -> Evaluator MeowAtom ()    #-}

-- Context references:
{-# SPECIALIZE newRef     :: Context MeowAtom -> Evaluator MeowAtom (Ref (Context MeowAtom))       #-}
{-# SPECIALIZE readRef    :: Ref (Context MeowAtom) -> Evaluator MeowAtom (Context MeowAtom)       #-}
{-# SPECIALIZE writeRef   :: Context MeowAtom -> Ref (Context MeowAtom) -> Evaluator MeowAtom ()   #-}
{-# SPECIALIZE modifyRef  :: (Context MeowAtom -> Context MeowAtom) -> Ref (Context MeowAtom) -> Evaluator MeowAtom () #-}

-- Context getters/setters:
{-# SPECIALIZE contextSearch :: Key -> Context MeowAtom -> Evaluator MeowAtom (Maybe (Ref MeowAtom)) #-}
{-# SPECIALIZE contextWrite  :: Key -> MeowAtom -> Context MeowAtom -> Evaluator MeowAtom ()         #-}
{-# SPECIALIZE localContext  :: Context MeowAtom -> Evaluator MeowAtom (Context MeowAtom)            #-}
{-# SPECIALIZE contextDefine :: Key -> Ref MeowAtom -> Context MeowAtom -> Evaluator MeowAtom ()     #-}
{-# SPECIALIZE contextMany   :: [(Key, Ref MeowAtom)] -> Context MeowAtom -> Evaluator MeowAtom ()   #-}
{-# SPECIALIZE initContext   :: Evaluator MeowAtom (Context MeowAtom)                                #-}
{-# SPECIALIZE freezeLocal   :: Context MeowAtom -> Evaluator MeowAtom (Context MeowAtom)            #-}

{--- Evaluator monad:
{-# SPECIALIZE (>>=)  :: Evaluator MeowAtom a -> (a -> Evaluator MeowAtom a) #-}
{-# SPECIALIZE (>>)   :: Evaluator MeowAtom a -> Evaluator MeowAtom a        #-}
{-# SPECIALIZE return :: a -> Evaluator MeowAtom a                           #-}

-- Evaluator functor:
{-# SPECIALIZE fmap   :: (a -> b) -> Evaluator MeowAtom a -> Evaluator MeowAtom b                       #-}
{-# SPECIALIZE pure   :: a -> Evaluator MeowAtom a                                                      #-}
{-# SPECIALIZE (<*>)  :: Evaluator MeowAtom (a -> b) -> Evaluator MeowAtom a -> Evaluator MeowAtom b    #-}

-- Evaluator context:
{-# SPECIALIZE lookUpRef  :: Key -> Evaluator MeowAtom (Maybe (Ref MeowAtom))                   #-}
{-# SPECIALIZE lookUp     :: Key -> Evaluator MeowAtom MeowAtom                                 #-}
{-# SPECIALIZE contextGet :: (Context MeowAtom -> a) -> Evaluator MeowAtom a                    #-}
{-# SPECIALIZE contextSet :: (Context MeowAtom -> Context MeowAtom) -> Evaluator MeowAtom ()    #-}
{-# SPECIALIZE runLocal   :: Evaluator MeowAtom a -> Context MeowAtom -> Evaluator MeowAtom ()  #-}
 -}
