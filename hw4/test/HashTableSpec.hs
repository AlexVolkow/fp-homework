module HashTableSpec where

import Test.Hspec
import HashTable
import Data.List (foldl')
import System.Random (randomRIO)
import qualified Data.Map.Lazy as Map

spec :: Spec
spec = do
  describe "HashTable" $ do
    it "simple" $ do
        ht <- newCHT
        sizeZero <- sizeCHT ht
        sizeZero `shouldBe` 0

        _ <- putCHT (2::Int) (3::Int) ht
        _ <- putCHT 3 4 ht

        v1 <- getCHT 2 ht
        v1 `shouldBe` Just 3

        v2 <- getCHT 3 ht
        v2 `shouldBe` Just 4

        size <- sizeCHT ht
        size `shouldBe` 2

    it "functional" $ do
        myht <- newCHT
        let ht = Map.empty
        testlog <- foldl' (\ht' _ -> runTest myht ht') (pure ht) [0..100000::Int]
        putStrLn (show testlog)


runTest :: ConcurrentHashTable Int Int -> IO (Map.Map Int Int) -> IO (Map.Map Int Int)
runTest myht htio = do
    ht <- htio
    doTest myht ht

doTest :: ConcurrentHashTable Int Int -> Map.Map Int Int -> IO (Map.Map Int Int)
doTest myht ht = do
    action <- randomRIO (0::Int, 2::Int)
    case (action) of
     0 -> do --size
        putStrLn ("size")
        size1 <- sizeCHT myht
        let size2 = Map.size ht
        size1 `shouldBe` size2
        pure ht

     1 -> do --get
        k <- randomRIO (0, 500::Int)
        putStrLn ("get (" ++ (show k) ++ ")")
        v1 <- getCHT k myht
        let v2 = Map.lookup k ht
        v1 `shouldBe` v2
        pure ht

     _ -> do  -- put
        v <- randomRIO (0, 1000::Int)
        k <- randomRIO (0, 500::Int)
        putStrLn ("put (" ++ (show k) ++ ", " ++ (show v) ++ ")")
        _ <- putCHT k v myht
        let newHt = Map.insert k v ht
        pure newHt