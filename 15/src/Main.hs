{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main where

import Data.Functor ((<&>))
import Data.Matrix (toLists, Matrix, fromLists, safeGet, setElem, mapPos)
import Data.Bifunctor (first, second)
import Data.List (foldl')
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    matrix <- readFile "data/map.txt" <&> fromLists . map (map readCell . init . tail) . init . tail . lines
    -- let matrix = m'
    directions <- readFile "data/movements.txt" <&> map readDir . concat . lines
    -- let directions = d'
-- run :: Matrix Cell -> (Int, Int) -> [Direction] -> Matrix Cell
    let robot = findRobot matrix
        finished = run matrix robot directions
    -- print matrix
    -- print robot
    -- print directions
    -- print finished
    print $ eval finished
    -- print directions

m' :: Matrix Cell
m' = fromLists $ map (map readCell . init . tail) $ init $ tail
    [ "########"
    , "#..O.O.#"
    , "##@.O..#"
    , "#...O..#"
    , "#.#.O..#"
    , "#...O..#"
    , "#......#"
    , "########"
    ]

m'' :: Matrix Cell
m'' = fromLists $ widen $ toLists m'

m''' :: Matrix Cell
m''' = fromLists $ map (map readCell . init . tail) $ init $ tail
    [ "########"
    , "#......#"
    , "#..[]..#"
    , "#.[][].#"
    , "#..[]..#"
    , "#..#...#"
    , "#......#"
    , "########"
    ]

d' :: [Direction]
d' = map readDir $ concat $ lines "<^^>>>vv<v>>v<<"

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

data Cell = EMPTY | BOX | WALL | ROBOT | BOXL | BOXR

instance Show Cell where
    show EMPTY = "."
    show BOX = "O"
    show WALL = "#"
    show ROBOT = "@"
    show BOXL = "["
    show BOXR = "]"

data PushV = VBox PushV | VBoxL PushV PushV | VBoxR PushV PushV | VEmpty | VWall
    deriving (Show)

data PushH = HBox PushH | HBoxLR PushH | HEmpty | HWall
    deriving (Show)

data Vertical = Up | Down
data Horizontal = Left | Right

readCell :: Char -> Cell
readCell '.' = EMPTY
readCell 'O' = BOX
readCell '#' = WALL
readCell '@' = ROBOT
readCell '[' = BOXL
readCell ']' = BOXR

get :: Direction -> (Int, Int) -> (Int, Int)
get UP = first (subtract 1)
get RIGHT = second (+ 1)
get DOWN = first (+ 1)
get LEFT = second (subtract 1)

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

-- eval :: Matrix Cell -> Int
eval = sum . catMaybes . toList . mapPos (\(x, y) c -> case c of { BOX -> Just (x * 100 + y); _ -> Nothing})

collectPushV :: Vertical -> Matrix Cell -> (Int, Int) -> PushV
collectPushV Up mat (x, y) = let up@(x', y') = (x - 1, y) in case safeGet x' y' mat of
    Nothing -> VWall
    (Just WALL) -> VWall
    (Just EMPTY) -> VEmpty
    (Just BOX) -> VBox $ collectPushV Up mat up
    (Just BOXL) -> VBoxL (collectPushV Up mat up) (collectPushV Up mat (x', y' + 1))
    (Just BOXR) -> VBoxR (collectPushV Up mat (x', y' - 1)) (collectPushV Up mat up)
    (Just ROBOT) -> VWall
collectPushV Down mat (x, y) = let down@(x', y') = (x + 1, y) in case safeGet x' y' mat of
    Nothing -> VWall
    (Just WALL) -> VWall
    (Just EMPTY) -> VEmpty
    (Just BOX) -> VBox $ collectPushV Down mat down
    (Just BOXL) -> VBoxL (collectPushV Down mat down) (collectPushV Down mat (x', y' + 1))
    (Just BOXR) -> VBoxR (collectPushV Down mat (x', y' - 1)) (collectPushV Down mat down)
    (Just ROBOT) -> VWall

collectPushH :: Horizontal -> Matrix Cell -> (Int, Int) -> PushH
collectPushH Main.Right mat (x, y) = let right@(x', y') = (x, y + 1) in case safeGet x' y' mat of
    Nothing -> HWall
    (Just WALL) -> HWall
    (Just EMPTY) -> HEmpty
    (Just BOX) -> HBox $ collectPushH Main.Right mat right
    (Just BOXL) -> HBoxLR $ collectPushH Main.Right mat (x', y' + 1)
    (Just BOXR) -> HBoxLR $ collectPushH Main.Right mat right
    (Just ROBOT) -> HWall
collectPushH Main.Left mat (x, y) = let left@(x', y') = (x, y - 1) in case safeGet x' y' mat of
    Nothing -> HWall
    (Just WALL) -> HWall
    (Just EMPTY) -> HEmpty
    (Just BOX) -> HBox $ collectPushH Main.Right mat left
    (Just BOXR) -> HBoxLR $ collectPushH Main.Right mat left
    (Just BOXL) -> HBoxLR $ collectPushH Main.Right mat (x', y' - 1)
    (Just ROBOT) -> HWall

widen :: [[Cell]] -> [[Cell]]
widen = map (concatMap helper) where
    helper :: Cell -> [Cell]
    helper EMPTY = [EMPTY, EMPTY]
    helper BOX = [BOXL, BOXR]
    helper WALL = [WALL, WALL]
    helper ROBOT = [ROBOT, EMPTY]
    helper BOXL = [BOXL, BOXR]
    helper BOXR = [BOXL, BOXR]

traversePushV :: Vertical -> (Int, Int) -> PushV -> 

getTreeV :: Vertical -> (Int, Int) -> PushV -> Maybe [(Cell, (Int, Int))]
getTreeV = ((sequence .) .) . helper where
    helper :: Vertical -> (Int, Int) -> PushV -> [Maybe (Cell, (Int, Int))]
    helper Up _ VEmpty = []
    helper Up _ VWall = [Nothing]
    helper Up pos@(x, y) (VBox up) = Just (BOX, pos) : helper Up (x - 1, y) up
    helper Up (x, y) (VBoxL upl upr) = Just (BOXL, (x - 1, y)) : Just (BOXR, (x - 1, y + 1)) : concatMap (uncurry (helper Up)) [((x - 1, y), upl), ((x - 1, y + 1), upr)]
    helper Up (x, y) (VBoxR upl upr) = Just (BOXL, (x - 1, y - 1)) : Just (BOXR, (x - 1, y)) : concatMap (uncurry (helper Up)) [((x - 1, y - 1), upl), ((x - 1, y), upr)]
    helper Down _ VEmpty = []
    helper Down _ VWall = [Nothing]
    helper Down pos@(x, y) (VBox up) = Just (BOX, pos) : helper Down (x + 1, y) up
    helper Down (x, y) (VBoxL upl upr) = Just (BOXL, (x + 1, y)) : Just (BOXR, (x + 1, y + 1)) : concatMap (uncurry (helper Down)) [((x + 1, y), upl), ((x + 1, y + 1), upr)]
    helper Down (x, y) (VBoxR upl upr) = Just (BOXL, (x + 1, y - 1)) : Just (BOXR, (x + 1, y)) : concatMap (uncurry (helper Down)) [((x + 1, y - 1), upl), ((x + 1, y), upr)]
