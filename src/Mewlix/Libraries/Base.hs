{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Mewlix.Libraries.Base
( baseLibrary
) where

import Mewlix.Abstract.Meow
import Mewlix.Data.Ref
import Mewlix.Data.Key (Key)
import Mewlix.Abstract.State
import Mewlix.Data.Stack (Stack)
import qualified Mewlix.Data.Stack as Stack
import Mewlix.Abstract.Prettify
import Mewlix.Abstract.Meowable
import Mewlix.Abstract.PrimLens
import Mewlix.Parser.AST
import Mewlix.Libraries.Utils
import Mewlix.Interpreter.Primitive
import Mewlix.Interpreter.Exceptions
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import qualified Data.HashMap.Strict as HashMap
import Lens.Micro.Platform ((%~), over)
import Data.Functor ((<&>))
-- IO:
import Mewlix.IO.Print (printText, printTextLn, printErrorLn)
import qualified Data.Text.IO as TextIO
import Mewlix.Utils.Time (clockSec)
import Control.Monad.IO.Class (MonadIO)
import System.Random (randomIO)
import Mewlix.IO.File
import Mewlix.IO.Directory

{- Base -}
baseLibrary :: (MonadIO m) => m (Environment MeowPrim)
baseLibrary = createEnvironment
    [ makeIFunc "meow"      ["x"]   meow
    , makeIFunc "purr"      ["x"]   purr
    , makeIFunc "listen"    [   ]   listen
    , makeIFunc "snoop"     [   ]   snoop
    , makeIFunc "search"    ["x"]   search
    , makeIFunc "angry"     ["x"]   angry
    , makeIFunc "reverse"   ["x"]   reverseFn
    , makeIFunc "sort"      ["x"]   sortFn
    , makeIFunc "int"       ["x"]   toInt
    , makeIFunc "float"     ["x"]   toFloat
    , makeIFunc "string"    ["x"]   toString
    , makeIFunc "nuzzle"    ["x"]   toBool
    , makeIFunc "bap"       ["x"]   toChar 
    , makeIFunc "bop"       ["x"]   fromChar
    , makeIFunc "keys"      ["x"]   getKeys
    , makeIFunc "values"    ["x"]   getValues
    , makeIFunc "copy"      ["x"]   meowCopy
    , makeIFunc "hash"      ["x"]   meowHash
    , makeIFunc "exists"    ["x"]   meowExist
    , makeIFunc "typeof"    ["x"]   meowTypeOf
    , makeIFunc "throw"     ["x"]   throwEx
    -- Math:
    , makeIFunc "pi"        [   ]   meowPi
    , makeIFunc "exp"       ["x"]   meowExp
    , makeIFunc "log"       ["x"]   meowLog
    , makeIFunc "sqrt"      ["x"]   meowSqrt
    , makeIFunc "sin"       ["x"]   meowSin
    , makeIFunc "cos"       ["x"]   meowCos
    , makeIFunc "tan"       ["x"]   meowTan
    , makeIFunc "asin"      ["x"]   meowAsin
    , makeIFunc "acos"      ["x"]   meowAcos
    , makeIFunc "atan"      ["x"]   meowAtan
    , makeIFunc "round"     ["x"]   meowRound
    , makeIFunc "floor"     ["x"]   meowFloor
    , makeIFunc "ceiling"   ["x"]   meowCeiling
    , makeIFunc "random"    ["x"]   meowRandom
    , makeIFunc "time"      ["x"]   meowTime
    -- Boxes --
    , makeIFunc "lookup"    ["box", "key"]  meowLookup
    , makeIFunc "haskey"    ["box", "key"]  meowHasKey
    -- Text --
    , makeIFunc "upper"     ["x"]                   meowUpper
    , makeIFunc "lower"     ["x"]                   meowLower
    , makeIFunc "trim"      ["x"]                   meowTrim
    , makeIFunc "split"     ["str", "token"]        meowSplit
    , makeIFunc "substring" ["str", "token", "len"] meowSubstr
    , makeIFunc "replace"   ["str", "token", "rep"] meowReplace
    -- IO --
    , makeIFunc "read_file"         ["path"]                meowRead
    , makeIFunc "write_file"        ["path", "contents"]    meowWrite
    , makeIFunc "append_file"       ["path", "contents"]    meowAppend
    , makeIFunc "move_file"         ["from", "to"]          meowMoveFile
    , makeIFunc "remove_file"       ["path"]                meowRemoveFile
    , makeIFunc "rename_file"       ["path", "name"]        meowRenameFile
    , makeIFunc "file_exists"       ["path"]                meowHasFile
    , makeIFunc "make_directory"    ["path"]                meowMakeDirectory
    , makeIFunc "move_directory"    ["from", "to"]          meowMoveDirectory
    , makeIFunc "remove_directory"  ["path"]                meowRemoveDirectory
    , makeIFunc "rename_directory"  ["path", "name"]        meowRenameDirectory
    , makeIFunc "directory_exists"  ["path"]                meowHasDirectory
    ]

----------------------------------------------------------
{- IO -}
----------------------------------------------------------
meow :: IFunc
meow = do
    lookUp "x" >>= showMeow >>= liftIO . printTextLn
    return MeowNil

purr :: IFunc
purr = do
    lookUp "x" >>= showMeow >>= liftIO . printText
    return MeowNil

listen :: IFunc
listen = do
    line <- liftIO TextIO.getLine
    toMeow line

snoop :: IFunc
snoop = do
    args <- asks (moduleArgs . evaluatorMeta)
    toMeow args

angry :: IFunc
angry = do
    lookUp "x" >>= showMeow >>= liftIO . printErrorLn
    return MeowNil

search :: IFunc
search = lookUp "x" >>= \case
    (MeowString x) -> do
        defmap <- asks (defineMap . evaluatorMeta)
        let key = unboxStr x
        hasKey <- contextHas key 
        (return . MeowBool) $ hasKey || HashMap.member key defmap
    x -> throwError =<< argumentTypeException "search" [x]

----------------------------------------------------------
{- Lists/Strings -}
----------------------------------------------------------
reverseFn :: IFunc
reverseFn = lookUp "x" >>= \case
    (MeowString x) -> (return . MeowString) $ (unboxStrL %~ Text.reverse) x
    (MeowStack x) -> (return . MeowStack) $ (unboxStackL %~ Stack.reverse) x
    x -> throwError =<< argumentTypeException "reverse" [x]

sortFn :: IFunc
sortFn = lookUp "x" >>= \case
    (MeowStack x) -> MeowStack . toBoxedStack <$> primSort (unboxStack x)
    x -> throwError =<< argumentTypeException "sort" [x]


----------------------------------------------------------
{- Conversion -}
----------------------------------------------------------
toString :: IFunc
toString = lookUp "x" >>= \case
    str@(MeowString _) -> return str
    other -> showMeow other <&> MeowString . toBoxedString

toInt :: IFunc
toInt = lookUp "x" >>= \case
    i@(MeowInt _)  -> return i
    (MeowFloat x)  -> (return . MeowInt . round) x
    (MeowString x) -> MeowInt <$> readInt (unboxStr x)
    (MeowBool x)   -> (return . MeowInt . fromEnum) x
    x -> throwError =<< argumentTypeException "int" [x]

readInt :: Text -> Evaluator Int
readInt txt = case Read.signed Read.decimal txt of
    (Left e)  -> (throwError . conversionException "int" . Text.pack) e
    (Right x) -> (return . fst) x

toFloat :: IFunc
toFloat = lookUp "x" >>= \case
    f@(MeowFloat _) -> return f
    (MeowInt x)     -> (return . MeowFloat . fromIntegral) x
    (MeowString x)  -> MeowFloat <$> readDouble (unboxStr x)
    (MeowBool x)    -> (return . MeowFloat . fromIntegral . fromEnum) x
    x -> throwError =<< argumentTypeException "float" [x]

readDouble :: Text -> Evaluator Double
readDouble txt = case Read.signed Read.double txt of
    (Left e)  -> (throwError . conversionException "double" . Text.pack) e
    (Right x) -> (return . fst) x

toBool :: IFunc
toBool = lookUp "x" <&> MeowBool . meowBool

toChar :: IFunc
toChar = lookUp "x" >>= \case
    (MeowInt x) -> toMeow (toEnum x :: Char)
    (MeowString x) -> if (Text.null . unboxStr) x
        then return MeowNil
        else (toMeow . Text.head . unboxStr) x
    x -> throwError =<< argumentTypeException "bap" [x]

fromChar :: IFunc
fromChar = lookUp "x" >>= \case
    (MeowString x) -> (return . MeowInt) $ if (Text.null . unboxStr) x
        then 0
        else (fromEnum . Text.head . unboxStr) x
    x -> throwError =<< argumentTypeException "bop" [x]


----------------------------------------------------------
{- Math -}
----------------------------------------------------------

meowPi :: IFunc
meowPi = (return . MeowFloat) pi

mathBase :: (Double -> Double) -> Text -> IFunc
mathBase f name = lookUp "x" >>= \case
    (MeowFloat x) -> (return . MeowFloat . f) x
    (MeowInt x)   -> (return . MeowFloat . f . fromIntegral) x
    x -> throwError =<< argumentTypeException name [x]

meowExp :: IFunc
meowExp = mathBase exp "exp"

meowLog :: IFunc
meowLog = mathBase log "log"

meowSqrt :: IFunc
meowSqrt = mathBase sqrt "sqrt"

meowSin :: IFunc
meowSin = mathBase sin "sin"

meowCos :: IFunc
meowCos = mathBase cos "cos"

meowTan :: IFunc
meowTan = mathBase tan "tan"

meowAsin :: IFunc
meowAsin = mathBase asin "asin"

meowAcos :: IFunc
meowAcos = mathBase acos "acos"

meowAtan :: IFunc
meowAtan = mathBase atan "atan"

floatTransformBase :: (Double -> Int) -> Text -> IFunc
floatTransformBase f name = lookUp "x" >>= \case
    (MeowFloat x) -> (return . MeowInt . f) x
    (MeowInt x)   -> (return . MeowInt . f . fromIntegral) x
    x -> throwError =<< argumentTypeException name [x]

meowRound :: IFunc
meowRound = floatTransformBase round "round"

meowFloor :: IFunc
meowFloor = floatTransformBase floor "floor"

meowCeiling :: IFunc
meowCeiling = floatTransformBase ceiling "ceiling"

meowRandom :: IFunc
meowRandom = MeowFloat <$> liftIO (randomIO :: IO Double)

meowTime :: IFunc
meowTime = MeowFloat <$> liftIO clockSec


----------------------------------------------------------
{- Text -}
----------------------------------------------------------
textTransformBase :: (Text -> Text) -> Text -> IFunc
textTransformBase f name = lookUp "x" >>= \case
    (MeowString x) -> (toMeow . f . unboxStr) x
    x -> throwError =<< argumentTypeException name [x]

meowUpper :: IFunc
meowUpper = textTransformBase Text.toUpper "upper"

meowLower :: IFunc
meowLower = textTransformBase Text.toLower "lower"

meowTrim :: IFunc
meowTrim = lookUp "x" >>= \case
    (MeowString x) -> (toMeow . Text.strip . unboxStr) x
    x -> throwError =<< argumentTypeException "trim" [x]

meowSubstr :: IFunc
meowSubstr = (,,) <$> lookUp "str" <*> lookUp "start" <*> lookUp "len" >>= \case
    (MeowString str, MeowInt start, MeowInt len) -> do
        (toMeow . Text.take len . Text.drop start . unboxStr) str
    (x, y, z) -> throwError =<< argumentTypeException "substring" [x, y, z]

meowSplit :: IFunc
meowSplit = (,) <$> lookUp "str" <*> lookUp "token" >>= \case
    (MeowString str, MeowString token) -> if (not . Text.null . unboxStr) token
        then toMeow $ Text.splitOn (unboxStr str) (unboxStr token)
        else throwError =<< argumentTypeException "split" [MeowString str, MeowString token]
    (x, y) -> throwError =<< argumentTypeException "split" [x, y]

meowReplace :: IFunc
meowReplace = (,,) <$> lookUp "str" <*> lookUp "token" <*> lookUp "rep" >>= \case
    (MeowString str, MeowString token, MeowString replacement) -> if (not . Text.null . unboxStr) token
        then toMeow $ Text.replace (unboxStr token) (unboxStr replacement) (unboxStr str)
        else throwError =<< argumentTypeException "replace" (map MeowString [str, token, replacement])
    (x, y, z) -> throwError =<< argumentTypeException "replace" [x, y, z]


----------------------------------------------------------
{- Boxes -}
----------------------------------------------------------
getKeys :: IFunc
getKeys = lookUp "x" >>= \case
    (MeowBox x) -> do
        keys <- (fmap HashMap.keys . readRef . getBox) x
        toMeow keys
    x -> throwError =<< argumentTypeException "keys" [x]

getValues :: IFunc
getValues = lookUp "x" >>= \case
    (MeowBox x) -> do
        values <- (readRef . getBox) x >>= mapM readRef . HashMap.elems
        (return . MeowStack . listToBoxedStack) values
    x -> throwError =<< argumentTypeException "values" [x]

-- Safe box lookup.
-- When a key isn't found, it returns MeowNil.
meowLookup :: IFunc
meowLookup = (,) <$> lookUp "box" <*> lookUp "key" >>= \case
    (MeowBox box, MeowString key) -> do
        boxmap <- (readRef . getBox) box
        case HashMap.lookup (unboxStr key) boxmap of
            (Just ref) -> readRef ref
            Nothing    -> return MeowNil
    (x, y) -> throwError =<< argumentTypeException "lookup" [x, y]

meowHasKey :: IFunc
meowHasKey = (,) <$> lookUp "box" <*> lookUp "key" >>= \case
    (MeowBox box, MeowString key) -> do
        boxmap <- (readRef . getBox) box
        (return . MeowBool) $ HashMap.member (unboxStr key) boxmap
    (x, y) -> throwError =<< argumentTypeException "haskey" [x, y]


----------------------------------------------------------
{- Reflection -}
----------------------------------------------------------
meowCopy :: IFunc
meowCopy = lookUp "x" >>= primCopy

meowHash :: IFunc
meowHash = lookUp "x" >>= fmap MeowInt . primHash

meowExist :: IFunc
meowExist = lookUp "x" >>= \case
    (MeowString key) -> MeowBool <$> contextHas (unboxStr key)
    x -> throwError =<< argumentTypeException "exists" [x]

meowTypeOf :: IFunc
meowTypeOf = lookUp "x" >>= \x -> (return . MeowString . toBoxedString) $ case x of
    (MeowString _)  -> "string"
    (MeowInt _)     -> "int"
    (MeowFloat _)   -> "float"
    (MeowBool _)    -> "bool"
    (MeowStack _)   -> "stack"
    (MeowBox _)     -> "box"
    (MeowFunc _)    -> "function"
    (MeowIFunc _)   -> "inner-function"
    MeowNil         -> "nothing"

----------------------------------------------------------
{- Exceptions -}
----------------------------------------------------------

throwEx :: IFunc
throwEx = lookUp "x" >>= \case
    (MeowString x) -> (throwError . catOnComputer . unboxStr) x
    x -> throwError =<< argumentTypeException "throw" [x]


----------------------------------------------------------
{- File IO -}
----------------------------------------------------------
meowRead :: IFunc
meowRead = lookUp "path" >>= \case
    (MeowString path) -> do
        localize <- localizePath <$> asks (modulePath . moduleInfo)
        let localPath = (localize . Text.unpack . unboxStr) path
        liftIO (safeReadFile localPath) >>= \case
            (Left exception) -> throwError =<< undefined
            (Right contents) -> toMeow contents
    x -> throwError =<< argumentTypeException "read_file" [x]

------ ### -------

type WriteCallback = Text -> FilePath -> IO (Either Text ())

writeFileBase :: Text -> WriteCallback -> IFunc
writeFileBase name f = (,) <$> lookUp "path" <*> lookUp "contents" >>= \case
    (MeowString path, MeowString contents) -> do
        localize <- localizePath <$> asks (modulePath . moduleInfo)
        let localPath = (localize . Text.unpack . unboxStr) path
        liftIO (f (unboxStr contents) localPath) >>= \case
            (Left exception) -> throwError =<< undefined
            (Right _)        -> return MeowNil
    (x, y) -> throwError =<< argumentTypeException name [x, y]

meowWrite :: IFunc
meowWrite = writeFileBase "write_file" safeWriteFile

meowAppend :: IFunc
meowAppend = writeFileBase "append_file" safeAppendFile


------ ### -------

type PathCallback a = FilePath -> IO (Either Text a)

pathActionBase :: (Meowable a) => Text -> PathCallback a -> IFunc
pathActionBase name f = lookUp "path" >>= \case
    (MeowString path) -> do
        localize <- localizePath <$> asks (modulePath . moduleInfo)
        let localPath = (localize . Text.unpack . unboxStr) path
        liftIO (f localPath) >>= \case
            (Left exception) -> throwError =<< undefined
            (Right a)        -> toMeow a
    x -> throwError =<< argumentTypeException name [x]

-- Files:
meowHasFile :: IFunc
meowHasFile = pathActionBase "file_exists" safeDoesFileExist

meowRemoveFile :: IFunc
meowRemoveFile = pathActionBase "remove_file" safeRemoveFile

-- Directories:
meowHasDirectory :: IFunc
meowHasDirectory = pathActionBase "directory_exists" safeDirectoryExists

meowMakeDirectory :: IFunc
meowMakeDirectory = pathActionBase "make_directory" safeMakeDirectory

meowRemoveDirectory :: IFunc
meowRemoveDirectory = pathActionBase "remove_directory" safeRemoveDirectory

------ ### -------

type MoveCallback = FilePath -> FilePath -> IO (Either Text ())

pathMoveBase :: Text -> MoveCallback -> IFunc
pathMoveBase name f = (,) <$> lookUp "from" <*> lookUp "to" >>= \case
    (MeowString a, MeowString b) -> do
        localize <- localizePath <$> asks (modulePath . moduleInfo) 
        let unpack = Text.unpack . unboxStr
        let pathFrom = (localize . unpack) a
        let pathTo = (localize . unpack) b
        liftIO (f pathFrom pathTo) >>= \case
            (Left exception) -> throwError =<< undefined
            (Right _)        -> return MeowNil
    (x, y) ->  throwError =<< argumentTypeException name [x, y]

meowMoveFile :: IFunc
meowMoveFile = pathMoveBase "move_file" safeMoveFile

meowMoveDirectory :: IFunc
meowMoveDirectory = pathMoveBase "move_directory" safeMoveDirectory

------ ### -------

type RenameCallback = FilePath -> String -> IO (Either Text ())

pathRenameBase :: Text -> RenameCallback -> IFunc
pathRenameBase name f = (,) <$> lookUp "path" <*> lookUp "name" >>= \case
    (MeowString path, MeowString n) -> do
        localize <- localizePath <$> asks (modulePath . moduleInfo)
        let unpack = Text.unpack . unboxStr
        let localPath = (localize . unpack) path
        liftIO (f localPath (unpack n)) >>= \case
            (Left exception) -> throwError =<< undefined
            (Right _)        -> return MeowNil
    (x, y) -> throwError =<< argumentTypeException name [x, y]

meowRenameFile :: IFunc
meowRenameFile = pathRenameBase "rename_file" safeRenameFile

meowRenameDirectory :: IFunc
meowRenameDirectory = pathRenameBase "rename_directory" safeRenameDirectory


{-
------ ### -------

meowInspect :: Evaluator Prim
meowInspect = lookUp "path" >>= \case
    (MeowString path) -> do
        local <- asks (localPath . Text.unpack . meowPath . meowState)
        let path' = (local . Text.unpack) path
        let makeObject exists files = (fmap MeowObject . liftIO . createObject)
                [ ( "exists", MeowBool exists                               )
                , ( "files" , MeowList (map (MeowString . Text.pack) files) ) ]
        let exception = badFile "In 'peek_directory'" [path]
        (liftIO . safeDirectoryExists) path' >>= \case
            (Left exc) -> throwError $ exception exc
            (Right True) -> (liftIO . safeInspectDirectory) path' >>= \case
                (Left exc) -> throwError $ exception exc
                (Right files) -> makeObject True files
            (Right False) -> makeObject False []
    x -> throwError =<< badArgs "peek_directory" [x]
 -}

