{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Bifunctor (first, second)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Lazy (HashMap, alter, delete, difference, empty, foldlWithKey', fromList, insert, lookup, mapWithKey, size, toList, unions, (!?))
import Data.Matrix (Matrix, fromLists, mapPos, safeGet, (!))
import Data.Maybe (fromMaybe, mapMaybe)
import Prelude hiding (lookup)

-- to allow HashMaps indexed by Edge
import Data.Hashable
import Data.List (nub, sort)
import GHC.Generics (Generic)


main :: IO ()
main = do
    mat <- readFile "data/input.txt" <&> fromLists . lines
    let regions = getRegions $ mapPos (liftA2 (.) (,) (checkNeighbours mat)) mat
    print $ liftA2 (,) (priceWith circumference) (priceWith sides) regions


-- final part

priceWith :: (HashMap Edge (Int, Bool) -> Int) -> [[(Int, Int)]] -> Int
priceWith f = sum . map (liftA2 (*) length (f . outerEdges))

-- for Part 1
circumference :: HashMap Edge (Int, Bool) -> Int
circumference = size

-- for Part 2
sides :: HashMap Edge (Int, Bool) -> Int
sides = size . joinEdges


-- general helper functions

dupe :: a -> (a, a)
dupe x = (x, x)

leastFixPoint :: (Eq a) => (a -> a) -> a -> a
leastFixPoint f x =
    let x' = f x
     in if x == x'
            then x
            else leastFixPoint f x'

invert :: (Hashable v) => HashMap k v -> HashMap v [k]
invert = foldlWithKey' helper empty
  where
    helper :: (Hashable v) => HashMap v [k] -> k -> v -> HashMap v [k]
    helper hAcc k v = alter (Just . (k :) . fromMaybe []) v hAcc


-- for collection regions

getNeighbours :: [(Int, Int) -> (Int, Int)]
getNeighbours =
    [ id
    , first (+ 1)
    , first (subtract 1)
    , second (+ 1)
    , second (subtract 1)
    ]

-- return which of the neighbouring fields are in the same region
checkNeighbours :: Matrix Char -> (Int, Int) -> Char -> [(Int, Int)]
checkNeighbours m pos c = map snd $ filter fst $ mapMaybe (helper m c) $ uncurry zip $ dupe $ getNeighbours <*> [pos]
  where
    helper :: Matrix Char -> Char -> ((Int, Int), (Int, Int)) -> Maybe (Bool, (Int, Int))
    helper m c = liftMaybeFromFst . first (((== c) <$>) . flip (uncurry safeGet) m)
      where
        liftMaybeFromFst :: (Maybe a, b) -> Maybe (a, b)
        liftMaybeFromFst (Nothing, _) = Nothing
        liftMaybeFromFst (Just x, y) = Just (x, y)

-- return the list of fields that are in the same region as the list elements
exploreRegion :: Matrix ((Int, Int), [(Int, Int)]) -> [(Int, Int)] -> [(Int, Int)]
exploreRegion = leastFixPoint . step
  where
    step :: Matrix ((Int, Int), [(Int, Int)]) -> [(Int, Int)] -> [(Int, Int)]
    step m = nub . sort . uncurry (++) . second (concatMap (snd . (m !))) . dupe

getRegions :: Matrix ((Int, Int), [(Int, Int)]) -> [[(Int, Int)]]
getRegions m = map snd $ toList $ invert $ foldl' (collect m) empty m
  where
    collect :: Matrix ((Int, Int), [(Int, Int)]) -> HashMap (Int, Int) (Int, Int) -> ((Int, Int), [(Int, Int)]) -> HashMap (Int, Int) (Int, Int)
    collect m acc (pos, _) = case acc !? pos of
        (Just _) -> acc
        Nothing ->
            let region = exploreRegion m [pos]
             in foldl' (\a p -> insert p pos a) acc region


-- edge handling

data Edge
    = H Int Int
    | V Int Int
    deriving (Eq, Generic, Show)

instance Hashable Edge

getEdges :: [(Int, Int) -> Edge]
getEdges =
    [ uncurry H
    , uncurry H . first (+ 1)
    , uncurry V
    , uncurry V . second (+ 1)
    ]

fromList1 :: (Hashable k) => [k] -> HashMap k Int
fromList1 = fromList . uncurry zip . second (map (const 1)) . dupe

-- an outer edge is an edge of a field that is not also the edge of another field
outerEdges :: [(Int, Int)] -> HashMap Edge (Int, Bool)
outerEdges lst =
    let hms = map (second (fromList1 . (getEdges <*>) . pure) . dupe) lst
        hFinal = unions $ map (\(p, es) -> difference es (unions $ map snd $ filter ((/= p) . fst) hms)) hms
     in mapWithKey (\e l -> (l, isCritical hFinal e)) hFinal

-- a horizontal (vertical) edge is critical if it can be continued up (left) as well as down (right)
-- this occurs when two fields are diagonally across each other, e.g.
--   +———————————+
--   | X   X   X |
--   |   +———+   |
--   | X |   | X |
--   |   +———+———+
--   | X   X |
--   +———————+
isCritical :: HashMap Edge Int -> Edge -> Bool
isCritical h (H x y) = case (lookup (V x (y + 1)) h, lookup (V (x - 1) (y + 1)) h) of
    (Just _, Just _) -> True
    _ -> False
isCritical h (V x y) = case (lookup (H (x + 1) y) h, lookup (H (x + 1) (y - 1)) h) of
    (Just _, Just _) -> True
    _ -> False


-- For Part 2

-- merge all continuous edge sequences into one long edge
-- don't merge across critical boundaries!
joinEdges :: HashMap Edge (Int, Bool) -> HashMap Edge (Int, Bool)
joinEdges = leastFixPoint step
  where
    step :: HashMap Edge (Int, Bool) -> HashMap Edge (Int, Bool)
    step h = let hList = toList h in fst $ foldl' fun (h, empty) $ (,) <$> hList <*> hList
      where
        fun :: (HashMap Edge (Int, Bool), HashMap Edge ()) -> ((Edge, (Int, Bool)), (Edge, (Int, Bool))) -> (HashMap Edge (Int, Bool), HashMap Edge ())
        fun acc@(h, blocked) ((e@(H x y), (l, critical)), (e'@(H x' y'), (l', critical')))
            | x == x' && y + l == y' && not critical = case (lookup e blocked, lookup e' blocked) of
                (Nothing, Nothing) -> (delete e' h & insert e (l + l', critical'), insert e () blocked & insert e' ())
                _ -> acc
        fun acc@(h, blocked) ((e@(V x y), (l, critical)), (e'@(V x' y'), (l', critical')))
            | y == y' && x + l == x' && not critical = case (lookup e blocked, lookup e' blocked) of
                (Nothing, Nothing) -> (delete e' h & insert e (l + l', critical'), insert e () blocked & insert e' ())
                _ -> acc
        fun acc _ = acc
