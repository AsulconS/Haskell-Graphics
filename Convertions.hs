module Convertions where

import BMatrix

type Binary = [Int]
type Decimal = Integer

negative' :: Int -> Int
negative' x  | x == 0 = 1
        | x == 1 = 0

and' :: Int -> Int -> Int
and' x y    | x == 1 && y == 1 = 1
            | otherwise = 0

xor' :: Int -> Int -> Int
xor' x y    | x == y = 0
            | otherwise = 1

binaryToInteger :: Binary -> Decimal
binaryToInteger b = sum [2 ^ (inM - x) | x <- [0..inM], (b!!x) /= 0]
    where inM = length b - 1

integerToBinary :: Decimal -> Binary
integerToBinary 0 = [0]
integerToBinary n = [0 | x <- [1..(128 - length body)]] ++ body
    where body = reverse (helper n)

helper :: Decimal -> Binary
helper 0 = []
helper n    | n `mod` 2 == 1 = 1 : helper (n `div` 2)
            | n `mod` 2 == 0 = 0 : helper (n `div` 2)

shiftLeft :: Binary -> Binary
shiftLeft b = [b!!i | i <- [1..inM]] ++ [0]
    where inM = length b - 1

binarySum :: Binary -> Binary -> Binary
binarySum xs ys
    | length xs < length ys = binarySum (replicate (length ys - length xs) 0 ++ xs) ys
    | length xs > length ys = binarySum xs (replicate (length xs - length ys) 0 ++ ys)
    | otherwise = case binarySum' xs ys of
                    (0, zs) -> zs
                    (1, zs) -> 1:zs
    where
        binarySum' :: [Int] -> [Int] -> (Int, [Int])
        binarySum' [] ys = (0, ys)
        binarySum' xs [] = (0, xs)
        binarySum' (x:xs) (y:ys) =  let (c, zs) = binarySum' xs ys
                                        (c', z) = (x + y + c) `divMod` 2
                                    in (c', z:zs)

binaryRest :: Binary -> Binary -> Binary
binaryRest xs ys = binarySum xs (map negative' ys)
