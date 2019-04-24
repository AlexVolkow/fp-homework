module InterpretatorTestSpec where

import ShParser (parseScript)
import Interpretator (runScript, ctxVars)
import Data.Either (fromRight)
import Text.Megaparsec
import Test.Hspec
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "InterpretateScript" $ do
   it "assign" $ do
    scriptFile <- readFile "resources/task1/basic1.sh"
    let script = fromRight [] (runParser parseScript "" scriptFile)
    ctx <- runScript script ["haskell", "best", "language"]
    let env = ctxVars ctx
    env `shouldBe` Map.fromList [("0","haskell"),("1","best"),("2","language"),("a","b"),("b","language"),("c","language")]

   it "differentAssign" $ do
    scriptFile <- readFile "resources/task1/differentAssign.sh"
    let script = fromRight [] (runParser parseScript "" scriptFile)
    ctx <- runScript script ["haskell", "best", "language"]
    let env = ctxVars ctx
    env `shouldBe` Map.fromList
        [("0","haskell"),("1","best"),("2","language"),("a","haskellbest"),("b","haskellbestB"),
         ("c","aaBBccDD"),("ssSS","aaBBccDD$fhaskellbestprivetpoka\\zhoka"),
         ("sss","aaBBccDD$fhaskellbestprivetpoka\\zhokaaaBBccDD$fhaskellbestprivetpoka\\zhokahaskellbest   haskellbestB")]

   it "double quote" $ do
    scriptFile <- readFile "resources/task2/basic1.sh"
    let script = fromRight [] (runParser parseScript "" scriptFile)
    ctx <- runScript script ["haskell", "best", "language"]
    let env = ctxVars ctx
    env `shouldBe` Map.fromList [("0","haskell"),("1","best"),("2","language"),
           ("bla","My string: best and language -> mystring"),("foo","mystring")]
