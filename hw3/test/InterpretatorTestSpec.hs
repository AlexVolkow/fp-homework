module InterpretatorTestSpec where

import Test.Hspec
import BaseParser
import Interpretator(runScript)
import Data.Either(fromRight)
import Text.Megaparsec
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Block1.interpretateScript" $ do
   it "basic" $ do
        scriptFile <- readFile "resources/task1/basic1.sh"
        let script = fromRight [] (runParser parseScript "" scriptFile)
        env <- runScript script ["haskell", "best", "language"]
        env `shouldBe` Map.fromList [("0","haskell"),("1","best"),("2","language"),("a","\"b\""),("b","language"),("c","language")]

   it "differentAssign" $ do
        scriptFile <- readFile "resources/task1/differentAssign.sh"
        let script = fromRight [] (runParser parseScript "" scriptFile)
        env <- runScript script ["haskell", "best", "language"]
        env `shouldBe` Map.fromList
            [("0","haskell"),("1","best"),("2","language"),("a","haskellbest"),("b","haskellbest\"B\""),
            ("c","\"aaBBccDD\""),("ssSS","\"aaBBccDD\"\"$f\"haskellbest\"privet\"\"poka\\zhoka\""),
            ("sss","\"aaBBccDD\"\"$f\"haskellbest\"privet\"\"poka\\zhoka\"\"aaBBccDD\"\"$f\"haskellbest\"privet\"\"poka\\zhoka\"haskellbest\"   \"haskellbest\"B\"")]
   it "double quote" $ do
        scriptFile <- readFile "resources/task2/basic1.sh"
        let script = fromRight [] (runParser parseScript "" scriptFile)
        env <- runScript script ["haskell", "best", "language"]
        env `shouldBe` Map.fromList [("0","haskell"),("1","best"),("2","language"),("bla","\"My string: \"best\" and \"language\" -> \"\"mystring\""),("foo","\"mystring\"")]
