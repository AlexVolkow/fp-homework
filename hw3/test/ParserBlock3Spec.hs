module ParserBlock3Spec where

import Test.Hspec
import ShParser
import Text.Megaparsec

spec :: Spec
spec = do
    describe "Block3.commands" $ do
      it "basic" $ do
          scriptFile <- readFile "resources/task3/basic1.sh"
          (runParser parseScript "" scriptFile) `shouldBe`
              Right [Command "echo" [[Text "Arguments:"],[Reference "1"],[Reference "2"],[Reference "3"]],
              Assign "bla" [Text "34"],Command "echo" [[Text "bla:"],[Reference "bla"]],Command "read" [[Text "i1"]],
              Command "read" [[Text "i2"]],Command "echo" [[Text "Read two lines:"],[Reference "i1",Text " and ",Reference "i2"]],
              Command "read" [[Text "n"],[Text"m"]],Command "echo" [[Text "-n"],[Text "Read"],[Text "a"],[Text "line"],
              [Text "with"],[Text "a"],[Text "word"],[Reference "n"]],Command "echo" [[Text " and"],[Text "rest"],[Text"of"],
              [Text "line:"],[Reference "m"]],Command "cd" [[Text ".."]],Command "pwd" [],Command "exit" [[Text "5"]],
              Command "echo" [[Text "not reachable"]]]

      it "delimetrs" $ do
          scriptFile <- readFile "resources/task3/delimetrCheck.sh"
          (runParser parseScript "" scriptFile) `shouldBe`
             Right [Assign "a" [Text "b"],Command "read" [[Text "a"],[Text "b"]],Assign "a" [Text "b"],
             Assign "read1" [Text "qweqwe"],Command "read" [[Text "a"],[Text "b"]],Assign "a1" [Text "b1"],
             Assign "echo1" [Text "echo"],Assign "read1" [Text "read1"],Command "read" [[Text "a"]],
             Command "echo" [[Text "-n"],[Text "bek"],[Text "kek"]],Command "echo" [[Text "kek"],[Text "bek"]],
             Command "echo" [[Text "-n"]],Command "echo" [[Text "a"]]]

      it "empty arguments" $ do
          scriptFile <- readFile "resources/task3/emptyArguments.sh"
          (runParser parseScript "" scriptFile) `shouldBe`
              Right [Command "echo" [],Command "echo" [[Text "-n"]]]
