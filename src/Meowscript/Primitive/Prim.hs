module Meowscript.Primitive.Prim
( MeowPrim(..)
, Meowcode
, MeowMachine
, BoxedString(..)
, BoxedList(..)
, BoxKey
, PrimRef
, MeowPairs(..)
, MeowFunction(..)
, boxString
, boxList
) where

import Meowscript.Primitive.MeowKey (Key)
import Data.Int (Int32)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef)

type Meowcode = Chunk MeowPrim
type MeowMachine a = Evaluator MeowPrim a

type BoxKey = Key
type PrimRef = IORef MeowPrim

data MeowPrim =
      MeowInt Int32
    | MeowFloat Double
    | MeowString BoxedString
    | MeowBool Bool
    | MeowList BoxedList
    | MeowBox (HashMap.HashMap BoxKey PrimRef)
    | MeowFunc MeowFunction
    | MeowNil

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

data BoxedList = BoxedList
    { unboxList :: [MeowPrim]
    , listLen   :: Int        }

instance Semigroup BoxedList where
    BoxedList u1 l1 <> BoxedList u2 l2 = BoxedList (u1 <> u2) (l1 + l2)

-- An utility newtype that'll be useful when constructing new boxes later!
newtype MeowPairs = MeowPairs { getPairs :: [(BoxKey, MeowPrim)] }

data MeowFunction = MeowFunction
    { funcName  :: Text.Text
    , funcArity :: Int
    , funcBody  :: Chunk MeowPrim
    , upValues  :: [PrimRef]      }


{- Utils -}
boxString :: Text.Text -> BoxedString
boxString x = BoxedString { unboxStr = x, strLen = Text.length x }

boxList :: [MeowPrim] -> BoxedList
boxList xs = BoxedList { unboxList = xs, listLen = length xs }
