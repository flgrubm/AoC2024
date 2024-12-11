{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main where

import Data.Bifunctor (first, second)
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Matrix (Matrix, fromLists, mapPos, safeGet, toList, (!))
import Data.Maybe (catMaybes, mapMaybe)

main :: IO ()
main = do
    topologyMap <- readFile "data/input.txt" <&> fromLists . map (map digitToInt) . lines
    let trailHeads = catMaybes $ toList $ mapPos (\pos i -> if i == 0 then Just pos else Nothing) topologyMap
        trailsPerTrailhead = map (getHikingPaths topologyMap) trailHeads
    print $ liftA2 (,) (evaluateTrailhead score) (evaluateTrailhead rating) trailsPerTrailhead

evaluateTrailhead :: ([[(Int, Int)]] -> Int) -> [[[(Int, Int)]]] -> Int
evaluateTrailhead = (sum .) . map

getDirections :: [(Int, Int) -> (Int, Int)]
getDirections =
    [ first (+ 1)
    , first (subtract 1)
    , second (+ 1)
    , second (subtract 1)
    ]

-- get all (uphill) hiking paths starting at the given point (this could be a trailhead or any other point!)
getHikingPaths :: Matrix Int -> (Int, Int) -> [[(Int, Int)]]
getHikingPaths m' pos' = map reverse $ getHikingPaths' [[pos']] m' pos'
  where
    getHikingPaths' :: [[(Int, Int)]] -> Matrix Int -> (Int, Int) -> [[(Int, Int)]]
    getHikingPaths' acc m pos =
        let currentHeight = m ! pos
            reachableCells = map fst $ filter ((== currentHeight + 1) . snd) $ mapMaybe (\(x, y) -> (,) <$> Just (x, y) <*> safeGet x y m) $ getDirections <*> [pos] :: [(Int, Int)]
         in if currentHeight < 8
                then concatMap (\p -> getHikingPaths' (map (p :) acc) m p) reachableCells
                else concatMap (\p -> map (p :) acc) reachableCells

-- Part 1
score :: [[(Int, Int)]] -> Int
score = length . nub . map last

-- Part 2
rating :: [[(Int, Int)]] -> Int
rating = length
