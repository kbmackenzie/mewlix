module Meowscript.Abstract.Atom
( MeowAtom(..)
, AtomRef
, InnerFunc
, BoxedString(..)
, BoxedStack(..)
, MeowPairs(..)
, MeowFunction(..)
, MeowIFunction(..)
, CatBox(..)
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

type AtomRef = Ref MeowAtom
type InnerFunc = Evaluator MeowAtom MeowAtom

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
    , ifunc         :: InnerFunc        }

{- Boxes -}
newtype CatBox = CatBox { getBox :: Ref (HashMap.HashMap Key AtomRef) }

{- Utils -}
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
