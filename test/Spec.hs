{-# LANGUAGE OverloadedStrings #-}

import Mewlix.Compiler
import Data.Text (Text)
import Test.Hspec
    ( Spec
    , it
    , describe
    , shouldBe
    , shouldSatisfy
    , hspec
    , before
    )
import Data.Either (isRight)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.ByteString as ByteString
import Prelude hiding (readFile)
import System.FilePath ((</>))
import System.FilePattern.Directory (getDirectoryFiles)

readFile :: FilePath -> IO Text
readFile = fmap TextEncoding.decodeUtf8 . ByteString.readFile

snippets :: [(Text, Text)] -> Spec
snippets = describe "parse mewlix snippets" . do
    let snippet :: (Text, Text) -> Spec
        snippet (input, expected) = do
            let preview = if Text.length input >= 50
                then Text.take 50 input `Text.append` "..."
                else input
            let description = "should parse the expression " ++ show preview
            let compile = compileJS emptyContext "expression"

            it description $
                compile input `shouldBe` Right expected
    mapM_ snippet

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

main :: IO ()
main = do
    yarnballs <- getYarnBalls
    hspec $ do
        describe "mewlix parser & compiler" (scripts yarnballs)
