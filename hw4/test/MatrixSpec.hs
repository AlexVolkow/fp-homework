module MatrixSpec where

import Test.Hspec
import Matrix (multiply)

spec :: Spec
spec = do
  describe "Matrix multiplication" $ do
     it "ordinary" $ do
        let a = [[1, 2], [3, 4], [5, 6]]
        let b = [[3, 4, 5], [6, 7, 8]]
        multiply a b `shouldBe` (Just [[15, 18, 21], [33, 40, 47], [51, 62, 73]])

     it "incompatible demensions" $ do
        let a = [[1, 2], [3, 4], [5, 6]]
        let b = [[3, 4, 5]]
        multiply a b `shouldBe` Nothing

     it "single matrix" $ do
        let a = [[1]]
        let b = [[2]]
        multiply a b `shouldBe` Just [[2]]

     it "matrix of zero" $ do
        let a = [[1, 2], [3, 4], [5, 6]]
        let b = [[0, 0, 0], [0, 0, 0]]
        multiply a b `shouldBe` (Just [[0, 0, 0], [0, 0, 0], [0, 0, 0]])