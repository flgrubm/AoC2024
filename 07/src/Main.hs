{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main where

import Data.Functor ((<&>))
import Data.List (foldl')

main :: IO ()
main = do
    candidates <- readFile "data/input.txt" <&> map readC . lines
    print (f1 candidates, f2 candidates)

data Operator = ADD | MULT | CONCAT
    deriving (Eq, Show)

newtype Candidate = C (Int, Int, [Int]) -- (Target, First, Rest)
    deriving (Eq, Show)

readC :: String -> Candidate
readC str = let ws = words str
                target = (read :: String -> Int) $ init $ head ws
                first = (read :: String -> Int) $ head $ tail ws
                rest = map (read :: String -> Int) $ tail $ tail ws
            in C (target, first, rest)

-- apply an operation on two integers, use with left-fold
applyOp :: Int -> (Int, Operator) -> Int
applyOp m (n, ADD) = m + n
applyOp m (n, MULT) = m * n
applyOp m (n, CONCAT) = (m * (10 ^ (1 + floor (logBase 10.0 (fromIntegral n :: Double)) :: Int))) + n

-- check whether a candidate can be made correct with the given operations
checkCandidate :: Candidate -> [Operator] -> Bool
checkCandidate (C (target, first, rest)) ops = target == foldl' applyOp first (zip rest ops)

-- check whether a candidate can be made correct with any operation
check :: [Operator] -> Candidate -> Bool
check ops c@(C (_, _, rest)) = any (uncurry checkCandidate . (,) c) $ allOps ops $ length rest

-- return all possible combinations of applying the given operators
allOps :: [Operator] -> Int -> [[Operator]]
allOps ops n
    | n < 1 = [[]]
    | otherwise = map (:) ops <*> allOps ops (n - 1)

-- return how many of the candidates can be made correct by applying any of the given operators
f :: [Operator] -> [Candidate] -> Int
f ops = sum . map (\(C (target, _, _)) -> target) . filter (check ops)

f1 :: [Candidate] -> Int
f1 = f [ADD, MULT]

f2 :: [Candidate] -> Int
f2 = f [ADD, MULT, CONCAT]
