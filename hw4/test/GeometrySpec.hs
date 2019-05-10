module GeometrySpec where

import Test.Hspec
import Geometry

spec :: Spec
spec = do
  describe "Geometry" $ do
     it "plus" $ do
        let a = Point 1 2
        let b = Point 3 7
        (a `plus` b) `shouldBe` (Point 4 9)

     it "minus" $ do
        let a = Point 1 2
        let b = Point 3 7
        (b `minus` a) `shouldBe` (Point 2 5)

     it "perimeter" $ do
        let polygon = [Point 5 0, Point 3 4, Point 2 3, Point 1 2]
        perimeter polygon `shouldBe` 11.772699034745349

     it "perimeter line" $ do
        let polygon = [Point 0 0, Point 2 0]
        perimeter polygon `shouldBe` 4

     it "doubleArea" $ do
        let polygon = [Point 5 0, Point 3 4, Point 2 3, Point 1 2]
        doubleArea polygon `shouldBe` 12

     it "doubleArea square" $ do
        let polygon = [Point 1 0, Point 1 1, Point 0 1, Point 0 0]
        doubleArea polygon `shouldBe` 2