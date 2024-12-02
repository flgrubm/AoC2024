{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main where

import Data.List (sort, transpose)

main :: IO ()
main =
    readFile "data/input.txt"
        >>= print . liftA2 (,) (uncurry f1) (uncurry f2) . (\(a : b : _) -> (a, b)) . transpose . map (map (read :: String -> Int) . words) . lines

f1 :: [Int] -> [Int] -> Int
f1 a b = sum $ zipWith (\x y -> if x > y then x - y else y - x) (sort a) (sort b)

f2 :: [Int] -> [Int] -> Int
f2 = ((sum . map fst . filter (uncurry (==))) .) . liftA2 (,)
