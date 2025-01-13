{-# LANGUAGE OverloadedStrings #-}

import qualified Mewlix.Test.Expressions as Expressions
import qualified Mewlix.Test.Scripts as Scripts
import qualified Mewlix.Test.Strings as Strings
import qualified Mewlix.Test.Assignment as Assignment
import qualified Mewlix.Test.YarnStrings as YarnStrings
import Test.Hspec (describe, hspec)

main :: IO ()
main = do
    yarnballs <- Scripts.getYarnBalls
    hspec $ do
        describe "basic yarnballs"   (Scripts.test yarnballs)
        describe "expressions"       Expressions.test
        describe "assignments"       Assignment.test
        describe "strings"           Strings.test
        describe "yarn strings"      YarnStrings.test
