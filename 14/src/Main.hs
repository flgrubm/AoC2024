{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor (bimap, first, second)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import Data.Matrix as M
import Data.Maybe (mapMaybe)


main :: IO ()
main = do
    robots <- readFile "data/input.txt" <&> map parseRobot . lines
    let p1 = collectQuadrants $ mapMaybe getQuadrant $ simulateN 100 robots
        -- get the first time when there are >= 161 clustered robots (161 is the number of clustered robots in the pattern that should be found (see in the bottom of this file); in practice, 4 would have been enough)
        -- every configuration repeats again (latest) after lcm(101, 103) = 101 * 103 steps, so a maximum of 101 * 103 repetitions needs to be simulated
        p2 = snd $ head $ dropWhile ((< 161) . countClusters . fst) $ take (101 * 103) $ iterate simulate1 (robots, 0)
    print (p1, p2)


-- utilities

-- split a list on an element
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn p = foldr (step p) [[]]
  where
    step p e acc@(h : rest)
        | p == e = [] : acc
        | otherwise = (e : h) : rest

swap :: (a, b) -> (b, a)
swap = uncurry (flip (,))


-- robots

newtype Robot = R ((Int, Int), (Int, Int))
    deriving (Show)

parseRobot :: String -> Robot
parseRobot str = let [[x, y], [vx, vy]] = map (splitOn ',' . drop 2) $ words str in R ((mod (read x) 101, mod (read y) 103), (read vx, read vy))

toPosition :: Robot -> (Int, Int)
toPosition (R ((x, y), _)) = (x, y)


-- movement

advanceN :: Int -> Robot -> Robot
advanceN n (R ((x, y), (vx, vy))) = R ((mod (x + n * vx) 101, mod (y + n * vy) 103), (vx, vy))

simulateN :: Int -> [Robot] -> [Robot]
simulateN n = map (advanceN n)

simulate1 :: ([Robot], Int) -> ([Robot], Int)
simulate1 = bimap (simulateN 1) (+ 1)


-- quadrants

data Quadrant = UR | UL | DL | DR -- U(p) / D(own) - R(ight) / L(eft)

getQuadrant :: Robot -> Maybe Quadrant
getQuadrant (R ((x, y), _))
    | x < 50 && y < 51 = Just UL
    | x > 50 && y < 51 = Just UR
    | x < 50 && y > 51 = Just DL
    | x > 50 && y > 51 = Just DR
    | otherwise = Nothing

collectQuadrants :: [Quadrant] -> Int
collectQuadrants = (\(ul, ur, dl, dr) -> ul * ur * dl * dr) . foldl' step (0, 0, 0, 0)
  where
    step (ul, ur, dl, dr) UL = (ul + 1, ur, dl, dr)
    step (ul, ur, dl, dr) UR = (ul, ur + 1, dl, dr)
    step (ul, ur, dl, dr) DL = (ul, ur, dl + 1, dr)
    step (ul, ur, dl, dr) DR = (ul, ur, dl, dr + 1)


-- clusters

-- count clustered robots, i.e. robots that are surrounded by other robots on all four sides
countClusters :: [Robot] -> Int
countClusters rs = let hm = HM.fromList $ map ((, ()) . toPosition) rs in length $ HM.mapMaybeWithKey (const . getAllNeighbours hm) hm
  where
    getAllNeighbours rs = traverse (rs HM.!?) . ([first (+ 1), first (subtract 1), second (+ 1), second (subtract 1)] <*>) . pure


-- visualization

data EmptyOrRobot = Empty | Robot

instance Show EmptyOrRobot where
    show Empty = [' ']
    show Robot = ['█']

toMatrix :: [Robot] -> M.Matrix EmptyOrRobot
toMatrix = foldl' step (M.matrix 103 101 (const Empty))
  where
    step = flip (M.setElem Robot . bimap (+ 1) (+ 1) . swap . toPosition) -- (+ 1) since matrices are indexed starting from 1, but

-- The pattern to look for is:
--
-- █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
-- █                                                           █
-- █                                                           █
-- █                                                           █
-- █                                                           █
-- █                             █                             █
-- █                           █ █ █                           █
-- █                         █ █ █ █ █                         █
-- █                       █ █ █ █ █ █ █                       █
-- █                     █ █ █ █ █ █ █ █ █                     █
-- █                         █ █ █ █ █                         █
-- █                       █ █ █ █ █ █ █                       █
-- █                     █ █ █ █ █ █ █ █ █                     █
-- █                   █ █ █ █ █ █ █ █ █ █ █                   █
-- █                 █ █ █ █ █ █ █ █ █ █ █ █ █                 █
-- █                     █ █ █ █ █ █ █ █ █                     █
-- █                   █ █ █ █ █ █ █ █ █ █ █                   █
-- █                 █ █ █ █ █ █ █ █ █ █ █ █ █                 █
-- █               █ █ █ █ █ █ █ █ █ █ █ █ █ █ █               █
-- █             █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █             █
-- █                 █ █ █ █ █ █ █ █ █ █ █ █ █                 █
-- █               █ █ █ █ █ █ █ █ █ █ █ █ █ █ █               █
-- █             █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █             █
-- █           █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █           █
-- █         █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █         █
-- █                           █ █ █                           █
-- █                           █ █ █                           █
-- █                           █ █ █                           █
-- █                                                           █
-- █                                                           █
-- █                                                           █
-- █                                                           █
-- █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
