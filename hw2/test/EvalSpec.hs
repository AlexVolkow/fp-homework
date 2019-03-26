module EvalSpec where

import Block2 (ArithmeticError (..), Expr (..), eval)
import Test.Hspec

spec :: Spec
spec = do
  describe "Block2.eval" $ do
   it "simple test" $ do
        eval (Sub (Mul (Const 2) (Const 3)) (Add (Const 5) (Const (-1)))) `shouldBe` (Right 2)
   it "cosnt" $ do
        eval (Const 69) `shouldBe` (Right 69)
   it "add" $ do
        eval (Add (Const 1) (Const 2)) `shouldBe` (Right 3)
   it "substract" $ do
        eval (Sub (Const 5) (Const 1)) `shouldBe` (Right 4)
   it "mul" $ do
        eval (Mul (Const 2) (Const 2)) `shouldBe` (Right 4)
   it "pow" $ do
        eval (Pow (Const 2) (Const 4)) `shouldBe` (Right 16)
   it "div" $ do
        eval (Div (Const 4) (Const 2)) `shouldBe` (Right 2)
   it "div by zero" $ do
        eval (Div (Const 2) (Const 0)) `shouldBe` (Left DivByZero)
   it "pow negative" $ do
        eval (Pow (Const 2) (Const (-1))) `shouldBe` (Left PowNegate)

