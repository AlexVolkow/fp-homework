module ParserBlock1Spec where

import Test.Hspec
import ShParser
import Data.Either (isLeft)
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
            Right [Assign "a" [Text "a"],Assign "d" [Text "q1@#%^&*)(=)+\\}{``}?32.,"],Assign "e" [Text "$0"],
            Assign "ssSS" [Text "$e"],Assign "sss" [Text "$ssSS"]]

   it "specialSymbols" $ do
        scriptFile <- readFile "resources/task1/specialSymbols.sh"
        (runParser parseScript "" scriptFile) `shouldBe`
            Right [Assign "a" [Text "\\"],Assign "d" [Text "$"],Assign "b" [Text "$"],Assign "c" [Text "\\"],
            Assign "f" [Text "()"],Assign "e" [Text "$"], Assign "z" [Text "\""]]

   it "should fail if uncorrect brecket" $ do
        (runParser parseScript "" "a=(") `shouldSatisfy` isLeft

   it "should fail if uncorrect slash" $ do
        (runParser parseScript "" "a=\\g") `shouldSatisfy` isLeft

   it "a lot of empty lines" $ do
       scriptFile <- readFile "resources/task1/aLotOfEmptyLines.sh"
       (runParser parseScript "" scriptFile) `shouldBe`
        Right [Assign "a" [Text "a"],Assign "b" [Text "b"],Assign "c" [Text "c"],Assign "d" [Text "d"],
            Assign "e" [Text "e"],Assign "f" [Text "f"],Assign "g" [Text "g"],Assign "h" [Text "h"],Assign "i" [Text "i"]]

   it "correctnes without quote" $ do
    scriptFile <- readFile "resources/task1/correctnesWithoutQuote.sh"
    (runParser parseScript "" scriptFile) `shouldBe`
            Right [Assign "lastCommand" [Reference "_"],Assign "a" [Text "/a/"],Assign "lastCommand1" [Reference "_"],
                Assign "b" [Text "/b"],Assign "c" [Text "4:20"],Assign "d" [Text "1+1.0"],Assign "e" [Text "/"],
                Assign "f" [Text "@",Reference "a",Text "%",Text "`"],
                Assign "g" [Text "@",Reference "a",Text "%",Text "a",Text "`",Text "a"],Assign "h" [Text "@#$%*@#"],
                Assign "bek" [Text "kek    ",Text "kek   ",Text "kek"],Assign "kek" [Reference "bek"],
                     Assign "b" [Reference "1"],Assign "b" [Reference "1"]]

   it "var names" $ do
    scriptFile <- readFile "resources/task1/varNames.sh"
    (runParser parseScript "" scriptFile) `shouldBe`
        Right [Assign "a" [Text "a"],Assign "B" [Text "b"],Assign "abc" [Text "e"],
        Assign "ABC" [Text "f"],Assign "___" [Text "d"],Assign "aaBBccDD" [Text "f"],
        Assign "_ef__efEFWFfef_" [Text "g"]]


