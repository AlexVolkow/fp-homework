module StringSumSpec where

import Block1 (stringSum)
import Test.Hspec

spec :: Spec
spec = do
  describe "Block1.stringSum" $ do
    it "one number" $ do
      stringSum "1" `shouldBe` (Just (1))

    it "one incorrect" $ do
      stringSum "1 2 a" `shouldBe` (Nothing)

    it "ordinary" $ do
      stringSum "1  2  3" `shouldBe` (Just (6))

    it "should failed if string invalid" $
      stringSum "6, 2, 3" `shouldBe` (Nothing)

    it "big test" $ do
      stringSum (mconcat (replicate 100000 "1 ") ++ mconcat (replicate 100000 "2 ")) `shouldBe` (Just (300000))

    it "empty string" $ do
      stringSum "" `shouldBe` (Just 0)

