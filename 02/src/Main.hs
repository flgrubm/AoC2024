{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main where

main :: IO ()
main =
    readFile "data/input.txt"
        >>= print . liftA2 (,) f1 f2 . map (map (read :: String -> Int) . words) . lines

data SafetyAcc
    = Init
    | Safe Int
    | Unsafe

-- encodes whether an "unsafe" condition is being/has been met
checkSafeStep :: Int -> SafetyAcc -> SafetyAcc
checkSafeStep m Init = Safe m
checkSafeStep _ Unsafe = Unsafe
checkSafeStep m (Safe n)
    | m < n && n < m + 4 = Safe m
    | otherwise = Unsafe

isSafe :: [Int] -> Bool
isSafe xs = case foldr checkSafeStep Init xs of
    Unsafe -> False
    _ -> True

-- creates a bitmask `:: [Bool]` of length `total` where exactly `min (total, toDrop)` elements are `False` (`total` choose `toDrop` many)
maskN :: Int -> Int -> [[Bool]]
maskN total toDrop
    | total < 1 = [[]]
    | toDrop < 1 = [replicate total True]
    | total <= toDrop = [replicate total False]
    | otherwise = (map (True :) (maskN (total - 1) toDrop)) ++ (map (False :) (maskN (total - 1) (toDrop - 1)))

-- applies a bitmask to a list of Integers
applyMask :: [Int] -> [Bool] -> [Int]
applyMask [] _ = []
applyMask _ [] = []
applyMask (x : xs) (b : bs) = if b then x : (applyMask xs bs) else applyMask xs bs

both :: (a -> b) -> (a, a) -> (b, b)
both g (x, y) = (g x, g y)

-- check whether a list of Integers is n-safe, i.e. removing some n elements makes it "safe"
-- note that n-safe implies (n - 1)-safe, so we only need to look at all the possibilities where exactly n elements have been removed (i.e. `maskN n`)
f :: Int -> [[Int]] -> Int
f n = length . filter (uncurry (||)) . map (both (or . (\xs -> map (isSafe . applyMask xs) (maskN (length xs) n))) . liftA2 (,) id reverse)

f1 :: [[Int]] -> Int
f1 = f 0

f2 :: [[Int]] -> Int
f2 = f 1
