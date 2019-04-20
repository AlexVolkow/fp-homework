module ParserBlock2Spec where

import Test.Hspec
import BaseParser
import Text.Megaparsec

spec :: Spec
spec = do
    describe "Block2.doubleQuote" $ do
      it "double quote" $ do
          scriptFile <- readFile "resources/task2/basic1.sh"
          (runParser parseScript "" scriptFile) `shouldBe`
              Right [Assign "foo" [Text "mystring"],Assign "bla" [Text "My string: ",Reference "1",Text " and ",Reference "2",Text " -> ",Reference "foo"]]
      it "boundary case" $ do
          scriptFile <- readFile "resources/task2/boundaryCase.sh"
          (runParser parseScript "" scriptFile) `shouldBe`
            Right [Assign "a" [Text " hello"],Assign "b" [Text "\\'"],Assign "c" [Text "\""],Assign "d" [Text "$"],Assign "f" [Text "         "],Assign "e" [Text "\\"]]
      it "dirPaths" $ do
           scriptFile <- readFile "resources/task2/dirPaths.sh"
           (runParser parseScript "" scriptFile) `shouldBe`
                Right [Assign "a" [Text ".//a/kek/bek",Text "/2313/",Text "////"]]