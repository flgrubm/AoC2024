{-
Copyright © 2024–2025 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE TupleSections #-}

module Main where

import Data.Functor ((<&>))
import Data.Matrix (toLists, Matrix, fromLists, safeGet, setElem, mapPos, nrows, ncols)
import Data.Bifunctor (first, second)
import Data.List (foldl', nub)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    inputMapList <- readFile "data/map.txt" <&> map ((readCell <$>) . init . tail) . init .tail . lines

    let matrix = fromLists inputMapList
        matrixW = fromLists $ widen inputMapList

    directions <- readFile "data/movements.txt" <&> map readDir . concat . lines

    let robot = findRobot matrix
        finished = run matrix robot directions

    let robotW = findWRobot matrixW
        finishedW = runW matrixW robotW directions
    print $ gps finished
    print $ gpsW finishedW


-- related to directions

data Direction = UP | RIGHT | DOWN | LEFT

instance Show Direction where
    show UP = "^"
    show RIGHT = ">"
    show DOWN = "v"
    show LEFT = "<"

readDir :: Char -> Direction
readDir '^' = UP
readDir '>' = RIGHT
readDir 'v' = DOWN
readDir '<' = LEFT

data Vertical = Up | Down
data Horizontal = Left | Right

get :: Direction -> (Int, Int) -> (Int, Int)
get UP = first (subtract 1)
get RIGHT = second (+ 1)
get DOWN = first (+ 1)
get LEFT = second (subtract 1)

getV :: Vertical -> (Int, Int) -> (Int, Int)
getV Up = get UP
getV Down = get DOWN

getH :: Horizontal -> (Int, Int) -> (Int, Int)
getH Main.Left = get LEFT
getH Main.Right = get RIGHT


-- related to Cell contents

data Cell = EMPTY | BOX | WALL | ROBOT

instance Show Cell where
    show EMPTY = "."
    show BOX = "O"
    show WALL = "#"
    show ROBOT = "@"

readCell :: Char -> Cell
readCell '.' = EMPTY
readCell 'O' = BOX
readCell '#' = WALL
readCell '@' = ROBOT

data WCell = WEMPTY | BOXL | BOXR | WWALL | WROBOT

instance Show WCell where
    show WEMPTY = "."
    show BOXL = "["
    show BOXR = "]"
    show WWALL = "#"
    show WROBOT = "@"

readWCell :: Char -> WCell
readWCell '.' = WEMPTY
readWCell '[' = BOXL
readWCell ']' = BOXR
readWCell '#' = WWALL
readWCell '@' = WROBOT

widen :: [[Cell]] -> [[WCell]]
widen = map (concatMap helper) where
    helper :: Cell -> [WCell]
    helper EMPTY = [WEMPTY, WEMPTY]
    helper BOX = [BOXL, BOXR]
    helper WALL = [WWALL, WWALL]
    helper ROBOT = [WROBOT, WEMPTY]

clearBoxW :: Matrix WCell -> (Int, Int) -> Matrix WCell
clearBoxW mat pos = mat & setElem WEMPTY pos & setElem WEMPTY (get RIGHT pos)

insertBoxW :: Matrix WCell -> (Int, Int) -> Matrix WCell
insertBoxW mat pos = mat & setElem BOXL pos & setElem BOXR (get RIGHT pos)

-- auxiliary functions

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

maybeBoth :: (Maybe a, Maybe b) -> Maybe (a, b)
maybeBoth (Just x, Just y) = Just (x, y)
maybeBoth _ = Nothing

-- Part 1

collectMovableBoxes :: Matrix Cell -> Direction -> (Int, Int) -> Maybe [(Int, Int)]
collectMovableBoxes = helper [] where
    helper :: [(Int, Int)] -> Matrix Cell -> Direction -> (Int, Int) -> Maybe [(Int, Int)]
    helper acc mat dir pos@(x, y) = case safeGet x y mat of
        (Just EMPTY) -> Just ((x, y) : acc)
        (Just BOX) -> helper ((x, y) : acc) mat dir (get dir pos)
        Nothing -> Nothing
        (Just WALL) -> Nothing

run :: Matrix Cell -> (Int, Int) -> [Direction] -> Matrix Cell
run mat pos = fst . foldl' move (mat, pos) where
    move :: (Matrix Cell, (Int, Int)) -> Direction -> (Matrix Cell, (Int, Int))
    move acc@(mat, pos) dir = let pos' = get dir pos in case collectMovableBoxes mat dir pos' of
      Nothing -> acc
      (Just boxes) -> ((foldl' helper mat boxes & setElem ROBOT pos') & setElem EMPTY pos, pos')
        where
            helper :: Matrix Cell -> (Int, Int) -> Matrix Cell
            helper matAcc p = setElem BOX p matAcc

findRobot :: Matrix Cell -> (Int, Int)
findRobot = fst . head . filter (isRobot . snd) . toList . mapPos (,) where
    isRobot :: Cell -> Bool
    isRobot ROBOT = True
    isRobot _ = False

gps :: Matrix Cell -> Int
gps = sum . catMaybes . toList . mapPos (\(x, y) c -> case c of { BOX -> Just (x * 100 + y); _ -> Nothing})

-- Part 2

findWRobot :: Matrix WCell -> (Int, Int)
findWRobot = fst . head . filter (isRobot . snd) . toList . mapPos (,) where
    isRobot :: WCell -> Bool
    isRobot WROBOT = True
    isRobot _ = False

gpsW :: Matrix WCell -> Int
gpsW = sum . catMaybes . toList . mapPos (\(x, y) c -> case c of { BOXL -> Just (x * 100 + (y + 1)); _ -> Nothing})

collectPushV' :: Vertical -> Matrix WCell -> (Int, Int) -> Maybe [(Int, Int)]
collectPushV' dir mat pos = let posl = getV dir pos
                                posr = get RIGHT posl
                            in case both (flip (uncurry safeGet) mat) (posl, posr) of
                                (Just BOXL, _) -> (posl :) <$> collectPushV' dir mat posl
                                (Just BOXR, Just WEMPTY) -> let posll = get LEFT posl in (posll :) <$> collectPushV' dir mat posll
                                (Just WEMPTY, Just BOXL) -> (posr :) <$> collectPushV' dir mat posr
                                (Just BOXR, Just BOXL) -> let posll = get LEFT posl in nub . (posll :) . (posr :) . uncurry (++) <$> maybeBoth (collectPushV' dir mat (get LEFT posl), collectPushV' dir mat posr)
                                (Just WEMPTY, Just WEMPTY) -> Just []
                                _ -> Nothing

collectPushV :: Vertical -> Matrix WCell -> (Int, Int) -> Maybe [(Int, Int)]
collectPushV dir mat pos = let pos' = getV dir pos in case uncurry safeGet pos' mat of
    (Just WEMPTY) -> Just []
    (Just BOXL) -> (pos' :) <$> collectPushV' dir mat pos'
    (Just BOXR) -> let pos'' = get LEFT pos' in (pos'' :) <$> collectPushV' dir mat pos''
    _ -> Nothing

-- vertical movement
moveVW :: Vertical -> Matrix WCell -> (Int, Int) -> (Matrix WCell, (Int, Int))
moveVW dir mat rpos = case collectPushV dir mat rpos of
    Nothing -> (mat, rpos)
    (Just ls) -> foldl' clearBoxW mat ls
        & setElem WEMPTY rpos
        & flip (foldl' insertBoxW) (getV dir <$> ls)
        & setElem WROBOT (getV dir rpos)
        & (, getV dir rpos)
  
collectPushH :: Horizontal -> Matrix WCell -> (Int, Int) -> Maybe [(Int, Int)]
collectPushH dir mat pos = let pos' = getH dir pos in case uncurry safeGet pos' mat of
    Just WEMPTY -> Just []
    Just BOXL -> (pos' :) <$> collectPushH dir mat (getH dir pos')
    Just BOXR -> let pos'' = getH dir pos' in (pos'' :) <$> collectPushH dir mat pos''
    _ -> Nothing

-- horizontal movement
moveHW :: Horizontal -> Matrix WCell -> (Int, Int) -> (Matrix WCell, (Int, Int))
moveHW dir mat rpos = case collectPushH dir mat rpos of
    Nothing -> (mat, rpos)
    (Just ls) -> foldl' clearBoxW mat ls
        & setElem WEMPTY rpos
        & flip (foldl' insertBoxW) (getH dir <$> ls)
        & setElem WROBOT (getH dir rpos)
        & (, getH dir rpos)

-- one-step movement
moveW :: Direction -> Matrix WCell -> (Int, Int) -> (Matrix WCell, (Int, Int))
moveW UP = moveVW Up
moveW DOWN = moveVW Down
moveW LEFT = moveHW Main.Left
moveW RIGHT = moveHW Main.Right

runW :: Matrix WCell -> (Int, Int) -> [Direction] -> Matrix WCell
runW = (((fst .) . foldl' (\(m, p) d -> moveW d m p)) .) . (,)
-- runW mat pos = fst . foldl' (\(m, p) d -> moveW d m p) (mat, pos)
