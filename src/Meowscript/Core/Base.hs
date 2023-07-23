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
import Control.Monad.Reader (asks, liftIO)
import Data.Functor ((<&>))
import Control.Monad ((>=>))
import System.Random (randomIO)
import Meowscript.Utils.Time (clockSec)

baseLibrary :: IO ObjectMap
baseLibrary = createObject
    [ ("meow"    , MeowIFunc  ["x"] meow      )
    , ("purr"    , MeowIFunc  ["x"] purr      )
    , ("listen"  , MeowIFunc  [   ] listen    )
    , ("snoop"   , MeowIFunc  [   ] snoop     )
    , ("search"  , MeowIFunc  ["x"] search    )
    , ("angry"   , MeowIFunc  ["x"] angry     )
    , ("reverse" , MeowIFunc  ["x"] reverseFn )
    , ("sort"    , MeowIFunc  ["x"] sortFn    )
    , ("int"     , MeowIFunc  ["x"] toInt     )
    , ("float"   , MeowIFunc  ["x"] toDouble  )
    , ("string"  , MeowIFunc  ["x"] toString  )
    , ("nuzzle"  , MeowIFunc  ["x"] toBool    )
    , ("bap"     , MeowIFunc  ["x"] toChar    )
    , ("bop"     , MeowIFunc  ["x"] fromChar  )
    , ("keys"    , MeowIFunc  ["x"] getKeys   )
    , ("values"  , MeowIFunc  ["x"] getValues )
    , ("copy"    , MeowIFunc  ["x"] meowCopy  )
    , ("hash"    , MeowIFunc  ["x"] meowHash  )
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
    , ("random"  , MeowIFunc  [   ] meowRand  )
    , ("time"    , MeowIFunc  [   ] meowTime  )
        -- Boxes --
    , ("lookup"             , MeowIFunc  ["box", "key"]          meowLookup     )
    , ("haskey"             , MeowIFunc  ["box", "key"]          meowHasKey     )
        -- File IO --
    , ("read_file"          , MeowIFunc  ["path"]                meowRead       )
    , ("write_file"         , MeowIFunc  ["path", "contents"]    meowWrite      )
    , ("append_file"        , MeowIFunc  ["path", "contents"]    meowAppend     )
    , ("move_file"          , MeowIFunc  ["a", "b"]              meowMvFile     )
    , ("remove_file"        , MeowIFunc  ["path"]                meowRmFile     )
    , ("rename_file"        , MeowIFunc  ["path", "name"]        meowRenFile    )
    , ("file_exists"        , MeowIFunc  ["path"]                meowHasFile    )
    , ("make_directory"     , MeowIFunc  ["path"]                meowMkDir      )
    , ("move_directory"     , MeowIFunc  ["a", "b"]              meowMvDir      )
    , ("remove_directory"   , MeowIFunc  ["path"]                meowRmDir      )
    , ("rename_directory"   , MeowIFunc  ["path", "name"]        meowRenDir     )
    , ("directory_exists"   , MeowIFunc  ["path"]                meowHasDir     )
    , ("peek_directory"     , MeowIFunc  ["path"]                meowInspect    )
        -- Text --
    , ("upper"              , MeowIFunc  ["x"]                   meowUpper      )
    , ("lower"              , MeowIFunc  ["x"]                   meowLower      )
    , ("trim"               , MeowIFunc  ["x"]                   meowTrim       )
    , ("split"              , MeowIFunc  ["str", "token"]        meowSplit      )
    , ("substring"          , MeowIFunc  ["str", "start", "len"] meowSubstr     )
    , ("replace"            , MeowIFunc  ["str", "token", "rep"] meowReplace    )]

{- IO -} 
----------------------------------------------------------
meow :: Evaluator Prim
meow = lookUp "x" >>= showMeow >>= (liftIO . printStrLn) >> return MeowLonely

purr :: Evaluator Prim
purr = lookUp "x" >>= showMeow >>= (liftIO . printStr) >> return MeowLonely

listen :: Evaluator Prim
listen = liftIO TextIO.getLine <&> MeowString

snoop :: Evaluator Prim
snoop = asks (MeowList . map MeowString . _meowArgs . fst)

search :: Evaluator Prim
search = lookUp "x" >>= \case
    (MeowString x) -> let hasDef def = Map.member def . _meowDefines . fst
        in MeowBool <$> ((||) <$> keyExists x <*> (asks . hasDef) x)
    x -> throwError =<< badArgs "search" [x]

angry :: Evaluator Prim
angry = lookUp "x" >>= showMeow >>= (liftIO . printErrLn) >> return MeowLonely

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

toChar :: Evaluator Prim
toChar = lookUp "x" >>= \case
    (MeowInt x) -> (return . MeowString . Text.singleton . toEnum) x
    (MeowString x) -> return $ if Text.null x
        then MeowLonely
        else (MeowString . Text.singleton . Text.head) x
    x -> throwError =<< badArgs "to_char" [x]

fromChar :: Evaluator Prim
fromChar = lookUp "x" >>= \case
    (MeowString x) -> if Text.null x
        then (return . MeowInt) 0
        else (return . MeowInt . fromEnum . Text.head) x
    x -> throwError =<< badArgs "from_char" [x]



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

meowTime :: Evaluator Prim
meowTime = MeowDouble <$> liftIO clockSec


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

meowCopy :: Evaluator Prim
meowCopy = lookUp "x" >>= primCopy

meowHash :: Evaluator Prim
meowHash = lookUp "x" >>= primHash <&> MeowInt

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
    (MeowString path) -> do 
        local <- asks (localPath . Text.unpack . _meowPath . fst)
        (liftIO . safeReadFile . local . Text.unpack) path >>= \case
            (Left exception) -> throwError (badFile "In 'read_file'" [path] exception)
            (Right contents) -> (return . MeowString) contents
    x -> throwError =<< badArgs "read_file" [x]

------ ### -------

type WriteCallback = Text.Text -> FilePath -> IO (Either Text.Text ())

inFnName :: Text.Text -> Text.Text
inFnName name = Text.concat [ "In '", name, "'" ]

writeBase :: Text.Text -> WriteCallback -> Evaluator Prim
writeBase name f = (,) <$> lookUp "path" <*> lookUp "contents" >>= \case
    (MeowString path, MeowString contents) -> do
        local <- asks (localPath . Text.unpack . _meowPath . fst)
        let path' = (local . Text.unpack) path
        liftIO (f contents path') >>= \case
            (Left exception) -> throwError $ badFile (inFnName name) [path] exception
            (Right _) -> (return . MeowString) contents
    (x, y) -> throwError =<< badArgs name [x, y]

meowWrite :: Evaluator Prim
meowWrite = writeBase "write_file" safeWriteFile

meowAppend :: Evaluator Prim
meowAppend = writeBase "append_file" safeAppendFile

------ ### -------

type ExistCallback = FilePath -> IO (Either Text.Text Bool)

existsBase :: Text.Text -> ExistCallback -> Evaluator Prim
existsBase name f = lookUp "path" >>= \case
    (MeowString path) -> do
        local <- asks (localPath . Text.unpack . _meowPath . fst)
        (liftIO . f . local . Text.unpack) path >>= \case
            (Left exception) -> throwError $ badFile (inFnName name) [path] exception
            (Right exists) -> (return . MeowBool) exists
    x -> throwError =<< badArgs name [x]

meowHasFile :: Evaluator Prim
meowHasFile = existsBase "file_exists" safeDoesFileExist

meowHasDir :: Evaluator Prim
meowHasDir = existsBase "directory_exists" safeDirectoryExists

------ ### -------

meowMkDir :: Evaluator Prim
meowMkDir = lookUp "path" >>= \case
    (MeowString path) -> do
        local <- asks (localPath . Text.unpack . _meowPath . fst)
        (liftIO . safeMakeDirectory . local . Text.unpack) path >>= \case
            (Left exception) -> throwError $ badFile "In 'make_directory'" [path] exception
            (Right _) -> return MeowLonely
    x -> throwError =<< badArgs "make_directory" [x]

------ ### -------

type MoveCallback = FilePath -> FilePath -> IO (Either Text.Text ())

moveBase :: Text.Text -> MoveCallback -> Evaluator Prim
moveBase name f = (,) <$> lookUp "a" <*> lookUp "b" >>= \case
    (MeowString a, MeowString b) -> do
        local <- asks (localPath . Text.unpack . _meowPath . fst)
        let pathA = (local . Text.unpack) a
        let pathB = (local . Text.unpack) b
        liftIO (f pathA pathB) >>= \case
            (Left exception) -> throwError $ badFile (inFnName name) [a, b] exception
            (Right _) -> return MeowLonely
    (x, y) -> throwError =<< badArgs name [x, y]

meowMvFile :: Evaluator Prim
meowMvFile = moveBase "move_file" safeMoveFile

meowMvDir :: Evaluator Prim
meowMvDir = moveBase "move_directory" safeMoveDir

------ ### -------

type RemoveCallback = FilePath -> IO (Either Text.Text ())

removeBase :: Text.Text -> RemoveCallback -> Evaluator Prim
removeBase name f = lookUp "path" >>= \case
    (MeowString path) -> do
        local <- asks (localPath . Text.unpack . _meowPath . fst)
        (liftIO . f . local . Text.unpack) path >>= \case
            (Left exception) -> throwError $ badFile (inFnName name) [path] exception
            (Right _) -> return MeowLonely
    x -> throwError =<< badArgs name [x]

meowRmFile :: Evaluator Prim
meowRmFile = removeBase "remove_file" safeRemoveFile

meowRmDir :: Evaluator Prim
meowRmDir = removeBase "remove_directory" safeRemoveDir

------ ### -------

type RenameCallback = FilePath -> String -> IO (Either Text.Text ())

renameBase :: Text.Text -> RenameCallback -> Evaluator Prim
renameBase name f = (,) <$> lookUp "path" <*> lookUp "name" >>= \case
    (MeowString path, MeowString newName) -> do
        local <- asks (localPath . Text.unpack . _meowPath . fst)
        let path' = (local . Text.unpack) path
        let newName' = (local . Text.unpack) newName
        liftIO (f path' newName') >>= \case
            (Left exception) ->  throwError $ badFile (inFnName name) [path, newName] exception
            (Right _) -> return MeowLonely
    (x, y) -> throwError =<< badArgs name [x, y]

meowRenFile :: Evaluator Prim
meowRenFile = renameBase "rename_file" safeRenameFile

meowRenDir :: Evaluator Prim
meowRenDir = renameBase "rename_directory" safeRenameDir

------ ### -------

meowInspect :: Evaluator Prim
meowInspect = lookUp "path" >>= \case
    (MeowString path) -> do
        local <- asks (localPath . Text.unpack . _meowPath . fst)
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
