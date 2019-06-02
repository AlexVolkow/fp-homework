module FSLensSpec where

import FSLens
import Test.Hspec
import System.FilePath (FilePath(..))
import Lens.Micro ((^?), (^..))

spec :: Spec
spec = do
  it "should return file name in directory" $ do
    let myDir = Dir "myDir" [Dir "A" [Dir "B" [ File "C" ]]]
    (myDir ^?  cd "A" . cd "B" . file "C") `shouldBe` Just "C"

  it "should return content of directory" $ do
    let myDir = Dir "myDir" [Dir "A" [Dir "B" [File "C", File "D" ]]]
    (myDir ^.. cd "A" . cd "B" . ls) `shouldBe` ["C", "D"]

  it "should change extention to pdf" $ do
    let myDir = Dir { _name = "myDir", _contents = [File "a.txt", File "b.txt", File "c.html"]}
    let newDir = chngext "pdf" myDir
    (newDir ^.. ls) `shouldBe` ["a.pdf", "b.pdf", "c.pdf"]

  it "should return all files and directories" $ do
    let myDir = Dir "myDir" [File "a.txt", Dir "A" [Dir "B" [File "C", File "D" ]]]
    (filenames myDir) `shouldBe` ["a.txt", "A", "B", "C", "D"]

  it "should remove empty directory" $ do
     let myDir = Dir "myDir" [File "a.txt", Dir "A" []]
     let newDir = removeIfEmpty "A" myDir
     (filenames newDir) `shouldBe` ["a.txt"]

  it "should't remove not empty directory" $ do
       let myDir = Dir "myDir" [File "a.txt", Dir "A" [File "B"]]
       let newDir = removeIfEmpty "A" myDir
       (filenames newDir) `shouldBe` ["a.txt", "A", "B"]
