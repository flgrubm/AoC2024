{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main where

import Data.List (tails, transpose)

main :: IO ()
main =
    readFile "data/input.txt"
        >>= print . liftA2 (,) f1 f2 . lines

f :: (a -> Int) -> ([String] -> [a]) -> [String] -> Int
f g h = sum . map g . h

-- Part 1

-- move every line progressively one more to the right than its predecessor, so that transposing the result will return precisely the diagonals going down and left
prepend :: [[Maybe a]] -> [[Maybe a]]
prepend [] = []
prepend (h : rest) = h : prepend (map (Nothing :) rest)

reduce :: Maybe a -> [a] -> [a]
reduce Nothing xs = xs
reduce (Just x) xs = x : xs

makeDiagonal :: ([[Maybe Char]] -> [[Maybe Char]]) -> [String] -> [String]
makeDiagonal g = map (foldr reduce []) . transpose . g . prepend . g . map (map Just)

directions :: [[String] -> [String]]
directions =
    [ id -- Left -> Right
    , map reverse -- Right -> Left
    , transpose -- Top -> Bottom
    , map reverse . transpose -- Bottom -> Top
    , makeDiagonal id -- Diagonal Down Left
    , map reverse . makeDiagonal id -- Diagonal Up Right
    , makeDiagonal reverse -- Diagonal Down Right
    , map reverse . makeDiagonal reverse -- Diagonal Up Left
    ]

getStringsAllDirections :: [String] -> [String]
getStringsAllDirections = concat . (directions <*>) . pure

countOccurences :: String -> Int
countOccurences [] = 0
countOccurences ('X' : 'M' : 'A' : 'S' : rest) = 1 + countOccurences rest
countOccurences (_ : rest) = countOccurences rest

f1 :: [String] -> Int
f1 = f countOccurences getStringsAllDirections

-- Part 2

data XShape = XShape Char Char Char Char Char

-- given three strings (here: list of length 3 of strings), return all n - 2 many X shapes, where n is the length of all three strings
collectXShapes :: [String] -> [XShape]
collectXShapes [a1 : a2 : a3 : restA, _ : b2 : b3 : restB, c1 : c2 : c3 : restC] = XShape b2 a1 a3 c1 c3 : collectXShapes [a2 : a3 : restA, b2 : b3 : restB, c2 : c3 : restC]
collectXShapes _ = []

getAllXShapes :: [String] -> [XShape]
getAllXShapes = concatMap (collectXShapes . take 3) . tails

checkXShape :: XShape -> Int
checkXShape (XShape 'A' 'M' 'M' 'S' 'S') = 1
checkXShape (XShape 'A' 'M' 'S' 'M' 'S') = 1
checkXShape (XShape 'A' 'S' 'M' 'S' 'M') = 1
checkXShape (XShape 'A' 'S' 'S' 'M' 'M') = 1
checkXShape _ = 0

f2 :: [String] -> Int
f2 = f checkXShape getAllXShapes
