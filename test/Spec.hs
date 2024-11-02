{-# LANGUAGE OverloadedStrings #-}

import Mewlix.Abstract.AST
import Mewlix.Abstract.Key
import Mewlix.Compiler
import Mewlix.Parser
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
import Prelude hiding (readFile, negate, concat, and, or)
import System.FilePath ((</>))
import System.FilePattern.Directory (getDirectoryFiles)

readFile :: FilePath -> IO Text
readFile = fmap TextEncoding.decodeUtf8 . ByteString.readFile

expressions :: [(Text, Expression)] -> Spec
expressions = describe "parse mewlix expressions" . do
    let expression :: (Text, Expression) -> Spec
        expression (input, expected) = do
            let preview = if Text.length input >= 50
                then Text.take 50 input `Text.append` "..."
                else input
            let description = "should parse the expression " ++ show preview
            let parse = parseExpr "expression"

            it description $
                parse input `shouldBe` Right expected
    mapM_ expression

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

basicExpressions :: [(Text, Expression)]
basicExpressions = 
    [ ("1 + 2"              , add (number 1) (number 2)                                    )
    , ("-1 + 2"             , add (negate (number 1)) (number 2)                           )
    , ("1 + 2 * 3"          , add (number 1) (multiply (number 2) (number 3))              )
    , ("(1 + 2) * 3"        , multiply (add (number 1) (number 2)) (number 3)              )
    , ("a .. b .. c"        , concat (concat (variable "a") (variable "b")) (variable "c") )
    , ("a..b..c"            , concat (concat (variable "a") (variable "b")) (variable "c") )
    , ("a push b"           , push (variable "a") (variable "b")                           )
    , ("a push b push c"    , push (push (variable "a") (variable "b")) (variable "c")     )
    , ("paw at a push b"    , push (peek (variable "a")) (variable "b")                    )
    , ("a and b or c"       , or (and (variable "a") (variable "b")) (variable "c")        )
    , ("a or b and c"       , or (variable "a") (and (variable "b") (variable "c"))        )
    ]
    where number   = PrimitiveExpr . MewlixInt
          add      = BinaryOperation Addition
          multiply = BinaryOperation Multiplication
          negate   = UnaryOperation Minus
          variable = Identifier . Key
          concat   = BinaryOperation StringConcat
          push     = BinaryOperation ShelfPush
          peek     = UnaryOperation ShelfPeek
          and      = BooleanAnd
          or       = BooleanOr

main :: IO ()
main = do
    yarnballs <- getYarnBalls
    hspec $ do
        describe "basic yarnballs"   (scripts yarnballs)
        describe "basic expressions" (expressions basicExpressions)
