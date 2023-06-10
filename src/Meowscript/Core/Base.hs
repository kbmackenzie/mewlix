{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE LambdaCase #-}

module Meowscript.Core.Base
( baseLibrary
) where

import Meowscript.Core.AST
import Meowscript.Core.Primitives
import Meowscript.Core.Environment
import Meowscript.Core.Keys
import Meowscript.Core.Pretty
import Meowscript.Core.Exceptions
import Meowscript.Utils.IO
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as Read
import Control.Monad.Except (throwError)
import Control.Monad.State (liftIO)
import Data.Functor ((<&>))
import Control.Monad ((>=>))
import System.Random (randomIO)

baseLibrary :: IO ObjectMap
baseLibrary = createObject
    [ ("meow"    , MeowIFunc  ["x"] meow      )
    , ("purr"    , MeowIFunc  ["x"] purr      )
    , ("listen"  , MeowIFunc  [   ] listen    )
    , ("reverse" , MeowIFunc  ["x"] reverseFn )
    , ("sort"    , MeowIFunc  ["x"] sortFn    )
    , ("int"     , MeowIFunc  ["x"] toInt     )
    , ("float"   , MeowIFunc  ["x"] toDouble  )
    , ("string"  , MeowIFunc  ["x"] toString  )
    , ("nuzzle"  , MeowIFunc  ["x"] toBool    )
    , ("keys"    , MeowIFunc  ["x"] getKeys   )
    , ("values"  , MeowIFunc  ["x"] getValues )
    , ("exists"  , MeowIFunc  ["x"] meowExist )
    , ("typeof"  , MeowIFunc  ["x"] typeOf    )
    , ("throw"   , MeowIFunc  ["x"] throwEx   )
    , ("pi"      , MeowIFunc  [   ] meowPi    )
    , ("exp"     , MeowIFunc  ["x"] meowExp   )
    , ("sqrt"    , MeowIFunc  ["x"] meowSqrt  )
    , ("log"     , MeowIFunc  ["x"] meowLog   )
    , ("sin"     , MeowIFunc  ["x"] meowSin   )
    , ("cos"     , MeowIFunc  ["x"] meowCos   )
    , ("tan"     , MeowIFunc  ["x"] meowTan   )
    , ("asin"    , MeowIFunc  ["x"] meowAsin  )
    , ("acos"    , MeowIFunc  ["x"] meowAcos  )
    , ("atan"    , MeowIFunc  ["x"] meowAtan  )
    , ("round"   , MeowIFunc  ["x"] meowRound )
    , ("ceiling" , MeowIFunc  ["x"] meowCeil  )
    , ("floor"   , MeowIFunc  ["x"] meowFloor )
        -- File IO --
    , ("read_file"   , MeowIFunc ["path"]             meowRead   )
    , ("write_file"  , MeowIFunc ["path", "contents"] meowWrite  )
    , ("append_file" , MeowIFunc ["path", "contents"] meowAppend )
        -- Boxes --
    , ("lookup"      , MeowIFunc  ["box", "key"]      meowLookup )
    , ("haskey"      , MeowIFunc  ["box", "key"]      meowHasKey )
        -- Text --
    , ("upper"       , MeowIFunc  ["x"]                   meowUpper   )
    , ("lower"       , MeowIFunc  ["x"]                   meowLower   )
    , ("random"      , MeowIFunc  [   ]                   meowRand    )
    , ("trim"        , MeowIFunc  ["x"]                   meowTrim    )
    , ("split"       , MeowIFunc  ["str", "token"]        meowSplit   )
    , ("substring"   , MeowIFunc  ["str", "start", "len"] meowSubstr  )
    , ("replace"     , MeowIFunc  ["str", "token", "rep"] meowReplace )]

{- IO -} 
----------------------------------------------------------
meow :: Evaluator Prim
meow = lookUp "x" >>= showMeow >>= (liftIO . printStrLn) >> return MeowLonely

purr :: Evaluator Prim
purr = lookUp "x" >>= showMeow >>= (liftIO . printStr) >> return MeowLonely

listen :: Evaluator Prim
listen = liftIO TextIO.getLine <&> MeowString

{- Lists/Strings -}
----------------------------------------------------------
reverseFn :: Evaluator Prim
reverseFn = lookUp "x" >>= \case
    (MeowList x) -> (return . MeowList . reverse) x
    (MeowString x) -> (return . MeowString . Text.reverse) x
    x -> throwError =<< badArgs "reverse" [x]

sortFn :: Evaluator Prim
sortFn = lookUp "x" >>= \case
    (MeowList x) -> MeowList <$> primSort x
    x -> throwError =<< badArgs "sort" [x]

{- Conversion -}
----------------------------------------------------------

toString :: Evaluator Prim
toString = lookUp "x" >>= showMeow <&> MeowString

toInt :: Evaluator Prim
toInt = lookUp "x" >>= \case
    a@(MeowInt _) -> return a
    (MeowDouble x) -> (return . MeowInt . round) x
    (MeowString x) -> MeowInt <$> readInt x
    (MeowBool x) -> (return . MeowInt . fromEnum) x
    x -> throwError =<< badArgs "int" [x]

readInt :: Text.Text -> Evaluator Int
readInt txt = case Read.signed Read.decimal txt of
    (Left er) -> throwError =<< badValue "int" (Text.pack er) [MeowString txt]
    (Right x) -> (return . fst) x

toDouble :: Evaluator Prim
toDouble = lookUp "x" >>= \case
    a@(MeowDouble _) -> return a
    (MeowInt x) -> (return . MeowDouble . fromIntegral) x
    (MeowString x) -> MeowDouble <$> readDouble x
    (MeowBool x) -> (return . MeowDouble . fromIntegral . fromEnum) x
    x -> throwError =<< badArgs "float" [x]

readDouble :: Text.Text -> Evaluator Double
readDouble txt = case Read.signed Read.double txt of
    (Left er) -> throwError =<< badValue "float" (Text.pack er) [MeowString txt]
    (Right x) -> (return . fst) x

toBool :: Evaluator Prim
toBool = lookUp "x" <&> (MeowBool . meowBool)


{- Math -}
----------------------------------------------------------

meowPi :: Evaluator Prim
meowPi = (return . MeowDouble) pi

mathFn :: (Double -> Double) -> Text.Text -> Evaluator Prim
mathFn fn name = lookUp "x" >>= \case
    (MeowDouble x) -> (return . MeowDouble . fn) x
    (MeowInt x) -> (return . MeowDouble . fn . fromIntegral) x
    x -> throwError =<< badArgs name [x]

meowExp :: Evaluator Prim
meowExp = mathFn exp "exp"

meowSqrt :: Evaluator Prim
meowSqrt = mathFn sqrt "sqrt"

meowLog :: Evaluator Prim
meowLog = mathFn log "log"

meowSin :: Evaluator Prim
meowSin = mathFn sin "sin"

meowCos :: Evaluator Prim
meowCos = mathFn cos "cos"

meowTan :: Evaluator Prim
meowTan = mathFn tan "tan"

meowAsin :: Evaluator Prim
meowAsin = mathFn asin "asin"

meowAcos :: Evaluator Prim
meowAcos = mathFn acos "acos"

meowAtan :: Evaluator Prim
meowAtan = mathFn atan "atan"

meowFloatFn :: (Double -> Int) -> Text.Text -> Evaluator Prim
meowFloatFn fn name = lookUp "x" >>= \case
    (MeowDouble x) -> (return . MeowInt . fn) x
    (MeowInt x) -> (return . MeowInt) x
    x -> throwError =<< badArgs name [x]

meowRound :: Evaluator Prim
meowRound = meowFloatFn round "round"

meowCeil :: Evaluator Prim
meowCeil = meowFloatFn ceiling "ceiling"

meowFloor :: Evaluator Prim
meowFloor = meowFloatFn floor "floor"

meowRand :: Evaluator Prim
meowRand = MeowDouble <$> liftIO (randomIO :: IO Double)


{- Text -}
----------------------------------------------------------
meowStrFn :: (Text.Text -> Text.Text) -> Text.Text -> Evaluator Prim
meowStrFn fn name = lookUp "x" >>= \case
    (MeowString x) -> (return . MeowString . fn) x
    x -> throwError =<< badArgs name [x]

meowUpper :: Evaluator Prim
meowUpper = meowStrFn Text.toUpper "upper"

meowLower :: Evaluator Prim
meowLower = meowStrFn Text.toLower "lower"

meowSubstr :: Evaluator Prim
meowSubstr = (,,) <$> lookUp "str" <*> lookUp "start" <*> lookUp "len" >>= \case
    (MeowString str, MeowInt start, MeowInt len) ->
        (return . MeowString . Text.take len . Text.drop start) str
    (x, y, z) -> throwError =<< badArgs "substring" [x, y, z]

meowSplit :: Evaluator Prim
meowSplit = (,) <$> lookUp "str" <*> lookUp "token" >>= \case
    (MeowString str, MeowString token) -> if (not . Text.null) token
        then (return . MeowList . (MeowString <$>)) (Text.splitOn token str)
        else throwError =<< badArgs "split" [MeowString str, MeowString token]
    (x, y) -> throwError =<< badArgs "split" [x, y]

meowTrim :: Evaluator Prim
meowTrim = lookUp "x" >>= \case
    (MeowString x) -> (return . MeowString . Text.strip) x
    x -> throwError =<< badArgs "trim" [x]

meowReplace :: Evaluator Prim
meowReplace = (,,) <$> lookUp "str" <*> lookUp "token" <*> lookUp "rep" >>= \case
    (MeowString str, MeowString token, MeowString replacement) -> if (not . Text.null) token
        then (return . MeowString) (Text.replace token replacement str)
        else throwError =<< badArgs "replace" (MeowString <$> [str, token, replacement])
    (x, y, z) -> throwError =<< badArgs "replace" [x, y, z]


{- Boxes -}
----------------------------------------------------------

getKeys :: Evaluator Prim
getKeys = lookUp "x" >>= \case
    (MeowObject x) -> (return . MeowList) (MeowString <$> Map.keys x)
    x -> throwError =<< badArgs "keys" [x]

getValues :: Evaluator Prim
getValues = lookUp "x" >>= \case
    (MeowObject x) -> MeowList <$> mapM (readMeowRef >=> ensureValue) (Map.elems x)
    x -> throwError =<< badArgs "values" [x]

meowLookup :: Evaluator Prim
meowLookup = (,) <$> lookUp "box" <*> lookUp "key" >>= \case
    (MeowObject box, MeowString key) -> case Map.lookup key box of
        (Just ref) -> readMeowRef ref
        Nothing -> return MeowLonely
    (x, y) -> throwError =<< badArgs "lookup" [x, y]

meowHasKey :: Evaluator Prim
meowHasKey = (,) <$> lookUp "box" <*> lookUp "key" >>= \case
    (MeowObject box, MeowString key) -> (return . MeowBool) (Map.member key box)
    (x, y) -> throwError =<< badArgs "haskey" [x, y]


{- Reflection -}
----------------------------------------------------------

meowExist :: Evaluator Prim 
meowExist = lookUp "x" >>= \case
    (MeowString key) -> MeowBool <$> keyExists key
    x -> throwError =<< badArgs "exists" [x]

typeOf :: Evaluator Prim
typeOf = lookUp "x" >>= \x -> return . MeowString $ case x of
    (MeowString _)  -> "string"
    (MeowInt _)     -> "int"
    (MeowDouble _)  -> "float"
    (MeowBool _)    -> "bool"
    (MeowList _)    -> "list"
    (MeowObject _)  -> "object"
    (MeowFunc {})   -> "function"
    (MeowIFunc {})  -> "inner-function"
    MeowLonely      -> "lonely"
    (MeowKey _)     -> "key" -- This shouldn't be evaluated, but alas.


{- Exceptions -}
----------------------------------------------------------
-- Allow users to throw their own exceptions.
-- This is shown a special exception, 'CatOnComputerException'.

throwEx :: Evaluator Prim
throwEx = lookUp "x" >>= \case
    (MeowString x) -> throwError (catOnComputer x)
    x -> throwError =<< badArgs "throw" [x]


{- File IO -}
----------------------------------------------------------

meowRead :: Evaluator Prim
meowRead = lookUp "path" >>= \case
    (MeowString path) -> (liftIO . safeReadFile . Text.unpack) path >>= \case
        (Left exception) -> throwError (badFile path "In 'read_file'" exception)
        (Right contents) -> (return . MeowString) contents
    x -> throwError =<< badArgs "read_file" [x]

meowWrite :: Evaluator Prim
meowWrite = (,) <$> lookUp "path" <*> lookUp "contents" >>= \case
    (MeowString path, MeowString contents) ->
        liftIO (safeWriteFile (Text.unpack path) contents) >>= \case
            (Left exception) -> throwError (badFile path  "In 'write_file'" exception)
            (Right _) -> (return . MeowString) contents
    (x, y) -> throwError =<< badArgs "write_file" [x, y]

meowAppend :: Evaluator Prim
meowAppend = (,) <$> lookUp "path" <*> lookUp "contents" >>= \case
    (MeowString path, MeowString contents) ->
        liftIO (safeAppendFile (Text.unpack path) contents) >>= \case
            (Left exception) -> throwError (badFile path  "In 'append_file'" exception)
            (Right _) -> (return . MeowString) contents
    (x, y) -> throwError =<< badArgs "append_file" [x, y]
