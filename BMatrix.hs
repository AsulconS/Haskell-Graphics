module BMatrix where

import Data.Array

type Vector a = Array Int a
type Matrix a = Array (Int, Int) a

listVector :: (Num a) => [a] -> Vector a
listVector xs = listArray (1, m) xs
    where m = length xs

listMatrix :: (Num a) => [[a]] -> Matrix a
listMatrix xss = listArray ((1, 1), (m, n)) (concat xss)
    where m = length xss
          n = length $ head xss

numRows :: (Num a) => Matrix a -> Int
numRows = fst . snd . bounds

numCols :: (Num a) => Matrix a -> Int
numCols = snd . snd . bounds

dimension :: (Num a) => Matrix a -> (Int, Int)
dimension m1 = (numRows m1, numCols m1)

divide :: Int -> [a] -> [[a]]
divide _ [] = []
divide n xs = take n xs : divide n (drop n xs)

matrixList :: (Num a) => Matrix a -> [[a]]
matrixList m1 = divide (numCols m1) (elems m1)

vectorList :: (Num a) => Vector a -> [a]
vectorList = elems

sum2Matrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
sum2Matrix m1 m2 = array ((1, 1), (m, n)) [((i, j), m1!(i, j) + m2!(i, j)) | i <- [1..m], j <- [1..n]]
    where (m, n) = dimension m1

rowMatrix :: (Num a) => Int -> Matrix a -> Vector a
rowMatrix i m1 = array (1, n) [(j, m1!(i,j)) | j <- [1..n]]
    where n = numCols m1

colMatrix :: (Num a) => Int -> Matrix a -> Vector a
colMatrix j m1 = array (1, m) [(i, m1!(i,j)) | i <- [1..m]]
    where m = numRows m1

scalarProd :: (Num a) => Vector a -> Vector a -> a
scalarProd v1 v2 = sum [i * j | (i, j) <- zip (elems v1) (elems v2)]

matrixProd :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixProd m1 m2 = array ((1, 1), (m, n)) [((i, j), scalarProd (rowMatrix i m1) (colMatrix j m2)) | i <- [1..m], j <- [1..n]]
    where m = numRows m1
          n = numCols m2

isMultipliable :: (Num a, Eq a) => Matrix a -> Matrix a -> Bool
isMultipliable m1 m2 = if ((numRows (transposed m1) == numRows m2 && numCols (transposed m1) == numCols m2)) then True else False

transposed :: (Num a) => Matrix a -> Matrix a
transposed m1 = array ((1, 1), (m, n)) [((i, j), m1!(j, i)) | i <- [1..m], j <- [1..n]]
    where (m, n) = dimension m1

isSquare :: (Num a) => Matrix a -> Bool
isSquare m1 = numRows m1 == numCols m1

isSymmetric :: (Num a, Eq a) => Matrix a -> Bool
isSymmetric m1 = m1 == transposed m1

mainDiag :: (Num a) => Matrix a -> Vector a
mainDiag m1 = array (1, n) [(i, m1!(i, i)) | i <- [1..n]]
    where n = min (numRows m1) (numCols m1)

secDiag :: (Num a) => Matrix a -> Vector a
secDiag m1 = array (1, n) [(i, m1!(i, m + 1 - i)) | i <- [1..n]]
    where n = min (numRows m1) (numCols m1)
          m = numCols m1

subMatrix :: (Num a) => Int -> Int -> Matrix a -> Matrix a
subMatrix i j m1 = array ((1, 1), (m - 1, n - 1)) [((k, l), m1 ! f k l) | k <- [1..m - 1], l <- [1..n - 1]]
    where (m, n) = dimension m1
          f k l | k < i  && l < j  = (k, l)
                | k >= i && l < j  = (k + 1, l)
                | k < i  && l >= j = (k, l + 1)
                | otherwise        = (k + 1, l + 1)

swapRows :: (Num a) => Int -> Int -> Matrix a -> Matrix a
swapRows k l m1 = array ((1, 1), (m, n)) [((i, j), m1 ! f i j) | i <- [1..m], j <- [1..n]]
    where (m, n) = dimension m1
          f i j | i == k    = (l, j)
                | i == l    = (k, j)
                | otherwise = (i, j)

swapCols :: (Num a) => Int -> Int -> Matrix a -> Matrix a
swapCols k l m1 = array ((1, 1), (m, n)) [((i, j), m1 ! f i j) | i <- [1..m], j <- [1..n]]
    where (m, n) = dimension m1
          f i j | j == k    = (i, l)
                | j == l    = (i, k)
                | otherwise = (i, j)

multRowBy :: (Num a) => Int -> a -> Matrix a -> Matrix a
multRowBy k x m1 = array ((1, 1), (m, n)) [((i, j), f i j) | i <- [1..m], j <- [1..n]]
    where (m, n) = dimension m1
          f i j | i == k    = x * (m1!(i, j))
                | otherwise = m1!(i, j)

sumRowRow :: (Num a) => Int -> Int -> Matrix a -> Matrix a
sumRowRow k l m1 = array ((1, 1), (m, n)) [((i, j), f i j) | i <- [1..m], j <- [1..n]]
    where (m, n) = dimension m1
          f i j | i == k    = m1!(i, j) + m1!(l, j)
                | otherwise = m1!(i, j)

sumRowBy :: (Num a) => Int -> Int -> a -> Matrix a -> Matrix a
sumRowBy k l x m1 = array ((1, 1), (m, n)) [((i, j), f i j) | i <- [1..m], j <- [1..n]]
    where (m, n) = dimension m1
          f i j | i == k    = m1!(i, j) + (x * m1!(l, j))
                | otherwise = m1!(i, j)

lookIndexFrom :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe Int
lookIndexFrom m1 j i | null xs   = Nothing
                     | otherwise = Just (head xs)
    where xs = [k | ((k, j'), y) <- assocs m1, j == j', y /= 0, k >= i]

lookPivotFrom :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe a
lookPivotFrom m1 j i | null xs   = Nothing
                     | otherwise = Just (head xs)
    where xs = [y | ((k, j'), y) <- assocs m1, j == j', y /= 0, k >= i]

cancelledColumnFrom :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
cancelledColumnFrom m1 j i = lookIndexFrom m1 j (i + 1) == Nothing

cancelElemColumnFrom :: (Fractional a, Eq a) => Matrix a -> Int -> Int -> Matrix a
cancelElemColumnFrom m1 j i = sumRowBy l i (-(m1!(l, j) / a)) m1
    where Just l = lookIndexFrom m1 j (i + 1)
          a      = m1!(i, j)

cancelColumnFrom :: (Fractional a, Eq a) => Matrix a -> Int -> Int -> Matrix a
cancelColumnFrom m1 j i | cancelledColumnFrom m1 j i = m1
                        | otherwise = cancelColumnFrom (cancelElemColumnFrom m1 j i) j i

elemNoNullsColFrom :: (Num a, Eq a) => Matrix a -> Int -> Int -> [a]
elemNoNullsColFrom m1 j i = [x | ((k, j'), x) <- assocs m1, x /= 0, j' == j, k >= i]

existsColNoNullFrom :: (Num a, Eq a) => Matrix a -> Int -> Int -> Bool
existsColNoNullFrom m1 j i = or [(not . null)(elemNoNullsColFrom m1 l i) | l <- [j..n]]
    where n = numCols m1

lessIndexColNoNullFrom :: (Num a, Eq a) => Matrix a -> Int -> Int -> Maybe Int
lessIndexColNoNullFrom m1 j i | null js   = Nothing
                              | otherwise = Just (head js)
    where n  = numCols m1
          js = [j' | j' <- [j..n], (not . null)(elemNoNullsColFrom m1 j' i)]

gaussAux :: (Fractional a, Eq a) => Matrix a -> Int -> Int -> Matrix a
gaussAux m1 i j
    | dimension m1 == (i, j)           = m1
    | not (existsColNoNullFrom m1 j i) = m1
    | otherwise                        = gaussAux m1' (i + 1) (j + 1)
    where Just j' = lessIndexColNoNullFrom m1 j i
          m2      = swapCols j j' m1
          Just i' = lookIndexFrom m2 j i
          m3      = swapRows i i' m2
          m1'     = cancelColumnFrom m3 j i

gauss :: (Fractional a, Eq a) => Matrix a -> Matrix a
gauss m1 = gaussAux m1 1 1

determinant :: (Fractional a, Eq a) => Matrix a -> a
determinant m1 = case (isSquare m1 && numRows m1 == 2) of
                    False -> (product . elems . mainDiag . gauss) m1
                    True  -> (product . mainDiag)(m1) - (product . secDiag)(m1)

attachmentVal :: (Fractional a, Eq a) => Matrix a -> Int -> Int-> a
attachmentVal m1 i j = pos i j * (determinant (subMatrix i j m1))
    where pos i j | (even i && odd j) || (odd i && even j) = -1
                  | otherwise = 1

attachments :: (Fractional a, Eq a) => Matrix a -> Matrix a
attachments m1 = array ((1, 1), (m, n)) [((i, j), attachmentVal m1 i j) | i <- [1..m], j <- [1..n]]
    where (m, n) = dimension m1

inverseMatrix :: (Fractional a, Eq a) => Matrix a -> a -> Matrix a
inverseMatrix m1 d = array ((1, 1), (m, n)) [((i, j), (ma!(i, j) / d)) | i <- [1..m], j <- [1..n]]
    where (m, n) = dimension m1
          ma = (attachments . transposed)(m1)

printMatrix :: (Fractional a, Eq a, Show a) => Matrix a -> IO ()
printMatrix m1 = case (matrixList m1) /= [] of
                    False -> return ()
                    True  -> do
                                print $ head (matrixList m1)
                                printMatrix $ listMatrix (tail (matrixList m1))

toString :: (Show a) => Vector a -> String
toString m1 = show $ elems m1
