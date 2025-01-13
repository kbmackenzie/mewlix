module Mewlix.Test.Scripts
( test
, getYarnBalls
) where

import Mewlix.Compiler (compileJS, emptyContext)
import Data.Text (Text)
import Test.Hspec
    ( Spec
    , it
    , describe
    , shouldSatisfy
    , before
    )
import Data.Either (isRight)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString as ByteString
import Prelude hiding (readFile)
import System.FilePath ((</>))
import System.FilePattern.Directory (getDirectoryFiles)

readFile :: FilePath -> IO Text
readFile = fmap TextEncoding.decodeUtf8 . ByteString.readFile

scripts :: [FilePath] -> Spec
scripts = do
    let script :: FilePath -> Spec
        script path = do
            let description = "should successfully compile script " ++ show path
            before (readFile path) $
                it description $ \contents -> do
                    let compile = compileJS emptyContext path
                    compile contents `shouldSatisfy` isRight
    describe "parse & compile scripts" . do
        mapM_ script

getYarnBalls :: IO [FilePath]
getYarnBalls = do
    let directory = "." </> "test" </> "YarnBalls"
    map (directory </>) <$> getDirectoryFiles directory ["*.mews"]

test :: [FilePath] -> Spec
test = scripts
