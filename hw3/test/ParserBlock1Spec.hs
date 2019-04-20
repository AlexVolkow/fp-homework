module ParserBlock1Spec where

import Test.Hspec
import BaseParser
import Data.Either(isLeft)
import Text.Megaparsec

spec :: Spec
spec = do
  describe "Block1.parseScript" $ do
   it "basic" $ do
        scriptFile <- readFile "resources/task1/basic1.sh"
        (runParser parseScript "" scriptFile) `shouldBe`
            Right [Assign "a" [Text "aaa $1 bb"],Assign "b" [Reference "2"],Assign "a" [Text "b"],Assign "c" [Reference "b"]]

   it "singleQuote" $ do
        scriptFile <- readFile "resources/task1/singleQuote.sh"
        (runParser parseScript "" scriptFile) `shouldBe`
            Right [Assign "a" [Text "a"],Assign "d" [Text "q1@#%^&*)(=)+\\}{``}?32.,"],Assign "e" [Text "$0"],Assign "ssSS" [Text "$e"],Assign "sss" [Text "$ssSS"]]

   it "specialSymbols" $ do
        scriptFile <- readFile "resources/task1/specialSymbols.sh"
        (runParser parseScript "" scriptFile) `shouldBe`
            Right [Assign "a" [Text "\\"],Assign "d" [Text "$"],Assign "b" [Text "$"],Assign "c" [Text "\\"],Assign "f" [Text "()"],Assign "e" [Text "$"]]

   it "should fail if uncorrect brecket" $ do
        (runParser parseScript "" "a=(") `shouldSatisfy` isLeft

   it "should fail if uncorrect slash" $ do
        (runParser parseScript "" "a=\\g") `shouldSatisfy` isLeft