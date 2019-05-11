module GaussSpec where

import Test.Hspec
import Gauss
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "Gauss" $ do
     it "ordinary 2x2" $ do
         let matrix = [[True, True], [True, False]]
         let b = [True, False]
         let solution = gauss matrix b
         solution `shouldBe` Just [False, True]
         verifySolution matrix b (fromJust solution) `shouldBe` True

     it "ordinary 3x3" $ do
        let matrix = [[True, True, False], [True, False, True], [False, True, False]]
        let b = [True, False, False]
        let solution = gauss matrix b
        solution `shouldBe` Just [True, False, True]
        verifySolution matrix b (fromJust solution) `shouldBe` True

     it "inconsistent matrix" $ do
        let matrix = [[True, True, False], [True, True, False], [False, True, False]]
        let b = [True, False, False]
        let solution = gauss matrix b
        solution `shouldBe` Nothing

     it "diagonal matrix" $ do
        let matrix = [[True, False, False], [False, True, False], [False, False, True]]
        let b = [True, True, True]
        let solution = gauss matrix b
        solution `shouldBe` Just [True, True, True]

     it "wrong solution" $ do
        let matrix = [[True, True, False], [True, False, True], [False, True, False]]
        let b = [True, False, False]
        verifySolution matrix b ([True, True, True]) `shouldBe` False

     it "not square" $ do
        let matrix = [[True, True, False], [True, False, True]]
        let b = [True, False]
        let solution = gauss matrix b
        solution `shouldBe` Nothing



