{-# LANGUAGE OverloadedStrings #-}

import qualified Mewlix.Test.Expressions as Expressions
import qualified Mewlix.Test.Scripts as Scripts
import Test.Hspec (describe, hspec)

main :: IO ()
main = do
    yarnballs <- Scripts.getYarnBalls
    hspec $ do
        describe "basic yarnballs"   (Scripts.test yarnballs)
        describe "basic expressions" Expressions.test
