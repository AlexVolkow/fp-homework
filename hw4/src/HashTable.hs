{-# LANGUAGE Strict #-}

module HashTable
    ( ConcurrentHashTable (..)
    , newCHT
    , putCHT
    , getCHT
    , sizeCHT
    ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Exception (mask_)
import Control.Monad (replicateM)
import Data.Array (Array, listArray, (!))
import Data.Hashable (Hashable, hash)

type Buckets k v = Array Int (TVar [(k,v)])

data ConcurrentHashTable  k v = HT
    {
    mtable     :: TVar (Buckets k v),
    mcount     :: TVar Int,
    mthreshold :: TVar Int
    }

newCHT  :: IO (ConcurrentHashTable k v)
newCHT = safeExecute $ do
    let size = 11
    slots <- replicateM size (newTVar [])
    let initTable = listArray (0, size - 1) slots
    tableVar <- newTVar initTable
    countVar <- newTVar 0
    thresholdVar <- newTVar size
    return (HT tableVar countVar thresholdVar)


putCHT :: (Hashable k, Eq k)  => Eq v => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value (HT table count threshold)  = safeExecute $ do
    size <- readTVar threshold
    currCount <- readTVar count
    let pos = hashfun key size
    x <- readTVar table
    slot <- readTVar $ x ! pos
    case lookup key slot of
     Just existing -> if (existing == value)
        then return ()
        else writeTVar (x ! pos) (map (\(k,v) -> if (k == key) then (key, value) else (k, v)) slot)

     Nothing -> if (loadFactor currCount size) then do
         let newSlot = (key, value) : slot
         writeTVar (x ! pos) newSlot
         writeTVar count (currCount + 1)
        else do
         rehashed <- resize x size
         let pos' = hashfun key (size * 2)
         slot' <- readTVar $ rehashed ! pos'
         let newSlot = (key, value) : slot'
         writeTVar (rehashed ! pos') (newSlot)
         writeTVar table rehashed
         writeTVar threshold (size * 2)
         writeTVar count (currCount + 1)

getCHT :: (Hashable k, Eq k)  => Eq v => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key (HT table _ threshold) = safeExecute $ do
    size <- readTVar threshold
    slot <- (\x -> readTVar $ x ! hashfun key size) =<< readTVar table
    return $ lookup key slot

sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT (HT _ count _) = safeExecute $ readTVar count

hashfun :: Hashable k => k -> Int -> Int
hashfun val tableSize = (hash val) `mod` tableSize

loadFactor :: Int -> Int -> Bool
loadFactor count tableSize = fromIntegral count * (0.75 :: Double) <= fromIntegral tableSize

copySlot :: (Hashable k, Eq k) => Eq v => [(k,v)] -> Buckets k v -> Int ->  STM ()
copySlot [] _ _ = return ()
copySlot ((key, value) : xs) newTable size = do
        let pos = hashfun key size
        list <- readTVar (newTable ! pos)
        let newSlot = (key, value) : list
        writeTVar (newTable ! pos) (newSlot)
        copySlot xs newTable size

resizeTable :: (Hashable k, Eq k)  => Eq v => Buckets k v -> Buckets k v -> Int -> Int -> STM (Buckets k v)
resizeTable _ newTable 0 _ = return newTable
resizeTable oldTable newTable oldSize newSize = do
        slot <- readTVar $ oldTable ! (oldSize - 1)
        if (not(null slot))
        then do
            copySlot slot newTable newSize
            resizeTable oldTable newTable (oldSize - 1) newSize
        else
            resizeTable oldTable newTable (oldSize - 1) newSize

resize :: (Hashable k, Eq k)  => Eq v => Buckets k v -> Int -> STM (Buckets k v)
resize oldTable oldSize = do
        let newSize = oldSize * 2
        table <- replicateM newSize (newTVar [])
        let newTable = listArray (0, newSize - 1) table
        bucket <- resizeTable oldTable newTable oldSize newSize
        return bucket

safeExecute :: STM a -> IO a
safeExecute v = mask_  (atomically v)

