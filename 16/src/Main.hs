{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (bimap, first, second)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sortOn, nub)

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Hashable
import Data.Matrix as M
import Data.Maybe (fromJust, catMaybes, mapMaybe)

main :: IO ()
main = do
    labyrinth <- readFile "data/input.txt" <&> fromRaw . fromLists . map ((readRawCell <$>) . init . tail) . init . tail . lines
    let res = dijkstra labyrinth
    case res of
        Nothing -> putStrLn "Endpoint unreachable"
        (Just (score, paths)) -> do
                                    print score
                                    print $ length $ paths
                                    print $ length $ getVisited paths
                                    print labyrinth
                                    print $ applyVisited labyrinth (getVisited paths)

mSmallRaw :: Matrix RawCell
mSmallRaw = fromLists $ map ((readRawCell <$>) . init . tail) $ init $ tail
    [ "###############"
    , "#.......#....E#"
    , "#.#.###.#.###.#"
    , "#.....#.#...#.#"
    , "#.###.#####.#.#"
    , "#.#.#.......#.#"
    , "#.#.#####.###.#"
    , "#...........#.#"
    , "###.#.#####.#.#"
    , "#...#.....#.#.#"
    , "#.#.#.###.#.#.#"
    , "#.....#...#.#.#"
    , "#.###.#.#.#.#.#"
    , "#S..#.....#...#"
    , "###############"
    ]

m2Raw :: Matrix RawCell
m2Raw = fromLists $ map ((readRawCell <$>) . init . tail) $ init $ tail
    [ "#################"
    , "#...#...#...#..E#"
    , "#.#.#.#.#.#.#.#.#"
    , "#.#.#.#...#...#.#"
    , "#.#.#.#.###.#.#.#"
    , "#...#.#.#.....#.#"
    , "#.#.#.#.#.#####.#"
    , "#.#...#.#.#.....#"
    , "#.#.#####.#.###.#"
    , "#.#.#.......#...#"
    , "#.#.###.#####.###"
    , "#.#.#...#.....#.#"
    , "#.#.#.#####.###.#"
    , "#.#.#.........#.#"
    , "#.#.#.#########.#"
    , "#S#.............#"
    , "#################"
    ]

mSmall :: Labyrinth
mSmall = fromRaw mSmallRaw

m2 :: Labyrinth
m2 = fromRaw m2Raw

mSmallMat :: Matrix Cell
mSmallMat = case mSmall of
    (L (mat, _, _)) -> mat

mSmallStart :: Coordinate
mSmallStart = case mSmall of
    (L (_, start, _)) -> start

mSmallEnd :: Coordinate
mSmallEnd = case mSmall of
    (L (_, _, end)) -> end

rSmall = getVisited $ snd $ fromJust $ dijkstra mSmall

r2 = getVisited $ snd $ fromJust $ dijkstra m2
r2' = fst $ fromJust $ dijkstra m2

data RawCell = RawEmpty | RawWall | RawStart | RawEnd

data Cell = Empty | Wall
    deriving (Eq)

newtype Labyrinth = L (Matrix Cell, Coordinate, Coordinate)

newtype Chart = C (Matrix Cell, Coordinate, Coordinate, HM.HashMap Position (Score, [Path]))
    deriving (Eq)

startHeading :: Heading
startHeading = East

startPosition :: Labyrinth -> Position
startPosition (L (_, start, _)) = Pos (start, startHeading)

instance Show RawCell where
    show RawEmpty = "."
    show RawWall = "#"
    show RawStart = "S"
    show RawEnd = "E"

instance Show Cell where
    show Empty = "."
    show Wall = "#"

readRawCell :: Char -> RawCell
readRawCell '.' = RawEmpty
readRawCell '#' = RawWall
readRawCell 'S' = RawStart
readRawCell 'E' = RawEnd

toRaw :: Labyrinth -> Matrix RawCell
toRaw (L (mat, XY start, XY end)) =
    mapPos (\_ -> \case Empty -> RawEmpty; Wall -> RawWall) mat
        & setElem RawStart start
        & setElem RawEnd end

fromRaw :: Matrix RawCell -> Labyrinth
fromRaw mat =
    let mat' = mapPos (\_ -> \case RawEmpty -> Empty; RawWall -> Wall; RawStart -> Empty; RawEnd -> Empty) mat
        start = fst $ head $ filter ((\case RawStart -> True; _ -> False) . snd) $ toList $ mapPos (,) mat
        end = fst $ head $ filter ((\case RawEnd -> True; _ -> False) . snd) $ toList $ mapPos (,) mat
     in L (mat', XY start, XY end)

instance Show Labyrinth where
    show = show . toRaw

instance Show Chart where
    show (C (mat, start, end, hm)) = show (L (mat, start, end)) ++ "\n" ++ show hm

data Heading = North | East | South | West
    deriving (Eq)

cvtHeading :: Heading -> Either (Maybe ()) (Maybe ())
cvtHeading North = Left Nothing
cvtHeading East = Left (Just ())
cvtHeading South = Right Nothing
cvtHeading West = Right (Just ())

-- use the auto-generated Hashable instance provided for Either (Maybe ()) (Maybe ())
instance Hashable Heading where
    hashWithSalt salt = hashWithSalt salt . cvtHeading

instance Show Heading where
    show North = "^"
    show East = ">"
    show South = "v"
    show West = "<"

data Movement = AdvanceLeft | AdvanceAhead | AdvanceRight | AdvanceBack
    deriving (Eq, Show)

newtype ID = ID [Movement]

isPrefix :: ID -> ID -> Bool
isPrefix (ID x) (ID y) = helper (reverse x) (reverse y)
  where
    helper :: [Movement] -> [Movement] -> Bool
    helper [] _ = True
    helper (x : xs) (y : ys)
        | x == y = helper xs ys
    helper _ _ = False

removePath :: HM.HashMap Position (Score, [ID]) -> IM.IntMap Path -> IS.IntSet -> (HM.HashMap Position (Score, [ID]), IM.IntMap Path, IS.IntSet)
removePath tiles paths ids = (tiles, paths, ids)

turn :: Movement -> Heading -> Heading
turn AdvanceLeft North = West
turn AdvanceLeft East = North
turn AdvanceLeft South = East
turn AdvanceLeft West = South
turn AdvanceAhead h = h
turn AdvanceRight North = East
turn AdvanceRight East = South
turn AdvanceRight South = West
turn AdvanceRight West = North
turn AdvanceBack x = turn AdvanceLeft $ turn AdvanceLeft x

goHead :: Heading -> Coordinate -> Coordinate
goHead North (XY (x, y)) = XY (x - 1, y)
goHead East (XY (x, y)) = XY (x, y + 1)
goHead South (XY (x, y)) = XY (x + 1, y)
goHead West (XY (x, y)) = XY (x, y - 1)

move :: Movement -> Position -> Score -> (Position, Score)
move AdvanceLeft (Pos (xy, h)) (S s) = let h' = turn AdvanceLeft h in (Pos (goHead h' xy, h'), S (s + 1001))
move AdvanceAhead (Pos (xy, h)) (S s) = (Pos (goHead h xy, h), S (s + 1))
move AdvanceRight (Pos (xy, h)) (S s) = let h' = turn AdvanceRight h in (Pos (goHead h' xy, h'), S (s + 1001))
move AdvanceBack (Pos (xy, h)) (S s) = let h' = turn AdvanceBack h in (Pos (goHead h' xy, h'), S (s + 2001))

currentPosScore :: Chart -> Path -> (Position, Score)
currentPosScore (C (_, start, _, hm)) path =
    let pos = currentPos (Pos (start, startHeading)) path
        score = fst $ fromJust $ HM.lookup pos hm
     in (pos, score)
  where
    currentPos :: Position -> Path -> Position
    currentPos pos (P []) = pos
    currentPos _ (P (x : _)) = x

newtype Score = S Int
    deriving (Eq, Ord, Show)

newtype Coordinate = XY (Int, Int)
    deriving (Eq, Show, Hashable)

newtype Position = Pos (Coordinate, Heading)
    deriving (Eq, Hashable, Show)

newtype Path = P [Position]
    deriving (Eq, Show)

canMoveTo :: Coordinate -> Matrix Cell -> Bool
canMoveTo (XY (x, y)) mat = case safeGet x y mat of
    (Just Empty) -> True
    _ -> False

appendPath :: Path -> Position -> Path
appendPath (P ps) p = P (p : ps)

advancePathDir :: Chart -> Path -> Movement -> (Chart, Maybe Path)
advancePathDir chart@(C (mat, start, end, hm)) path mv =
    let (pos, s) = currentPosScore chart path
        (pos'@(Pos (xy', _)), s') = move mv pos s
        path' = appendPath path pos'
    in if not $ canMoveTo xy' mat then (chart, Nothing)
    else case HM.lookup pos' hm of
        Nothing -> (C (mat, start, end, HM.insert pos' (s', [path']) hm), Just path')
        (Just (s'', _)) | s' < s'' -> (C (mat, start, end, HM.insert pos' (s', [path']) hm), Just path')
        (Just (s'', ps)) | s' == s'' -> (C (mat, start, end, HM.insert pos' (s', path' : ps) hm), Just path')
        _ -> (chart, Nothing)

-- second :: (b -> d) -> (a, b) -> (a, d)
-- second f (x, y) = (x, f y)

advancePath :: Chart -> Path -> (Chart, [Path])
advancePath chart path = second catMaybes $ foldl' (\(ch, ps) mv -> case advancePathDir ch path mv of (ch', p) -> (ch', p : ps)) (chart, []) [AdvanceLeft, AdvanceAhead, AdvanceRight]

advanceOnce :: Chart -> [Path] -> (Chart, [Path])
advanceOnce chart lives = second concat $ foldl' (\(ch, pss) p -> case advancePath ch p of (ch', ps) -> (ch', ps : pss)) (chart, []) lives

initializeChart :: Labyrinth -> Chart
initializeChart (L (mat, start, end)) = C (mat, start, end, HM.fromList [(Pos (start, startHeading), (S 0, [P []]))])

genDijkstra :: Labyrinth -> Chart
genDijkstra lab =
    let ch = initializeChart lab
        startPos = startPosition lab
        (ch', maybeBack) = advancePathDir ch (P [startPos]) AdvanceBack -- going straight back (i.e. two left turns) can only be a viable option in the very beginning
        (ch'', ps) = advancePath ch' (P [startPos])
    in helper ch'' (case maybeBack of {(Just p) -> p : ps; Nothing -> ps})
      where
        helper :: Chart -> [Path] -> Chart
        helper chart [] = chart
        helper chart lives = let (chart', lives') = advanceOnce chart lives in helper chart' lives'

dijkstra :: Labyrinth -> Maybe (Score, [Path])
dijkstra lab = case genDijkstra lab of
    (C (_, _, end, hm)) -> case sortOn fst $ catMaybes [HM.lookup (Pos (end, heading)) hm | heading <- [North, East, South, West]] of
        [] -> Nothing
        sps@((s, _) : _) -> Just (s, concatMap snd $ filter ((s ==) . fst) sps)

getVisited :: [Path] -> [Coordinate]
getVisited = nub . map (\(Pos (xy,_)) -> xy) . concatMap (\(P ps) -> ps)

data VCell = VEmpty | VWall | VVisited

instance Show VCell where
    show VEmpty = "."
    show VWall = "#"
    show VVisited = "O"

applyVisited :: Labyrinth -> [Coordinate] -> Matrix VCell
applyVisited (L (mat, _, _)) = foldl' (\m (XY xy) -> setElem VVisited xy m) (mapPos (const helper) mat)
    where
        helper :: Cell -> VCell
        helper Wall = VWall
        helper Empty = VEmpty
