{-# LANGUAGE InstanceSigs #-}

module Block2
    ( Expr (..)
    , ArithmeticError (..)
    , eval
    , moving
    ) where

import Control.Applicative (liftA2)
import Control.Monad.State (State, evalState, get, put)

--Task 1

data Expr
    = Const Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    deriving Eq

instance Show Expr where
    show (Const x) = show x
    show (Add l r) = show l ++ " + " ++ show r
    show (Sub l r) = show l ++ " - " ++ show r
    show (Mul l r) = show l ++ " * " ++ show r
    show (Div l r) = show l ++ " / " ++ show r
    show (Pow l r) = show l ++ " ^ " ++ show r

data ArithmeticError
  = PowNegate
  | DivByZero
  deriving Show

eval :: Expr -> Either ArithmeticError Int
eval (Const x)  = pure x
eval (Add l r)  = liftA2 (+) (eval l) (eval r)
eval (Sub l r)  = liftA2 (-) (eval l) (eval r)
eval (Mul l r)  = liftA2 (*) (eval l) (eval r)
eval (Div l r)  = case eval r of
    (Right 0) -> Left DivByZero
    (Right x) -> liftA2 div (eval l) (Right x)
    v         -> v
eval (Pow l r)  = case eval r of
    (Right y) -> if y < 0
                   then Left PowNegate
                   else liftA2 (^) (eval l) (Right y)
    v -> v

--Task 2

type MovingState = (Int, Int, [Int], [Float])

moving :: Int -> [Int] -> [Float]
moving n arr = reverse $ evalState (moving' arr) (1, 0, arr, [])
    where
        moving':: [Int] -> State MovingState [Float]
        moving' [] = do
            (_, _, _, sma) <- get
            return sma

        moving' (x:xs) = do
            (idx, avgSum, minusIdx, sma) <- get
            let nextSum = if (idx > n) then avgSum + x - (head minusIdx) else avgSum + x
            let nextAverage = ((realToFrac nextSum) / (realToFrac (min idx n))) : sma
            if (idx > n)
            then
                put (idx + 1, nextSum, tail minusIdx, nextAverage)
            else
                put (idx + 1, nextSum, minusIdx, nextAverage)
            moving' xs

