module ParserSpec where

import Block3 (element, eof, ok, parseBrackets, parseInt, parseListOfList, runParser, satisfy,
               stream)
import Data.Maybe (isJust, isNothing)
import Test.Hspec

spec :: Spec
spec = do
  describe "Block3.parsers" $ do
    it "ok" $ do
      runParser ok "i love haskell" `shouldSatisfy` isJust
      runParser ok "" `shouldSatisfy` isJust

    it "eof" $ do
      runParser eof "" `shouldSatisfy` isJust
      runParser eof "void" `shouldSatisfy` isNothing

    it "satisfy" $ do
      runParser (satisfy (== 'h')) "helloh" `shouldBe` Just ('h', "elloh")
      runParser (satisfy (== 'h')) "b" `shouldSatisfy` isNothing

    it "element" $ do
      runParser (element 'p') "prefixsuffix" `shouldBe` Just ('p', "refixsuffix")
      runParser (element 'a') "bc" `shouldSatisfy` isNothing

    it "stream" $ do
      runParser (stream "prefix") "prefixsuffix" `shouldBe` Just ("prefix", "suffix")
      runParser (stream "prefix") "suffix" `shouldSatisfy` isNothing

    it "parseBrackets" $ do
      runParser parseBrackets "()" `shouldSatisfy` isJust
      runParser parseBrackets "" `shouldSatisfy` isJust
      runParser parseBrackets "(())()" `shouldSatisfy` isJust
      runParser parseBrackets "(())" `shouldSatisfy` isJust
      runParser parseBrackets ")" `shouldSatisfy` isNothing
      runParser parseBrackets "(()" `shouldSatisfy` isNothing

    it "parseInt" $ do
      runParser parseInt "69" `shouldBe` Just (69, "")
      runParser parseInt "+69" `shouldBe` Just (69, "")
      runParser parseInt "-69" `shouldBe` Just (-69, "")
      runParser parseInt "0000069" `shouldBe` Just (69, "")
      runParser parseInt "abc" `shouldBe` Nothing
      runParser parseInt "-abc" `shouldBe` Nothing
      runParser parseInt "+abc" `shouldBe` Nothing

    it "parseListOfList" $ do
      runParser parseListOfList "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
      runParser parseListOfList "3, 1" `shouldBe` Nothing

