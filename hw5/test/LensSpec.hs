module LensSpec where

import Lens
import Test.Hspec
import Data.Function ((&))

spec :: Spec
spec = do
  it "view" $ do
    (3, 7) ^. _1 `shouldBe` 3
    (3, 7) ^. _2 `shouldBe` 7
  it "set" $ do
    ((3, 7) & _2 .~ 69) `shouldBe` (3, 69)
    ((3, 7) & _1 .~ 69) `shouldBe` (69, 7)
  it "over" $ do
    ((0, 0) & _1 %~ (+ 1)) `shouldBe` (1, 0)
    ((0, 0) & _2 %~ (+ 1)) `shouldBe` (0, 1)