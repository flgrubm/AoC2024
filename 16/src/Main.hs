{-
Copyright © 2025 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE LambdaCase #-}

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad
import Control.Monad.Extra (loopM)
import Data.List (sortOn, foldl', nub)
import Control.Applicative (empty)

import qualified Data.Matrix as Mat
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Bifunctor (first, second, bimap)
import Data.Functor ((<&>))
import GHC.Utils.Panic (panic)

main :: IO ()
main = do
    lab <- readFile "data/input.txt" <&> fromRawLabyrinth . readRawLabyrinth
    case evalDijkstra lab of
        (Right res) -> print res
        _ -> putStrLn "End position unreachable from the start"


-- Labyrinth

data RawCell = RawEmpty | RawWall | RawStart | RawEnd
data Cell = Empty | Wall

readRawLabyrinth :: String -> RawLabyrinth
readRawLabyrinth = Mat.fromLists . map (map readRawCell .init . tail) . init . tail . lines
    where
        readRawCell :: Char -> RawCell
        readRawCell '.' = RawEmpty
        readRawCell '#' = RawWall
        readRawCell 'S' = RawStart
        readRawCell 'E' = RawEnd
        readRawCell _ = panic "Syntax Error"

fromRawLabyrinth :: RawLabyrinth -> Labyrinth
fromRawLabyrinth rlab =
    let lab = Mat.mapPos (const fromRawCell) rlab
        start = getFirstOfRawCellPattern (\case {RawStart -> True; _ -> False}) rlab
        end = getFirstOfRawCellPattern (\case {RawEnd -> True; _ -> False}) rlab
    in (lab, start, end)
    where
        fromRawCell :: RawCell -> Cell
        fromRawCell RawWall = Wall
        fromRawCell _ = Empty

        getFirstOfRawCellPattern :: (RawCell -> Bool) -> RawLabyrinth -> Coordinate
        getFirstOfRawCellPattern fb = fst . head . filter (fb . snd) . Mat.toList . Mat.mapPos (,)


-- navigation types

type Coordinate = (Int, Int)

data Heading = N | E | S | W
    deriving (Eq)

-- use automatically generated hash implementation of Either (Maybe ()) (Maybe ()) ≃ Heading
instance Hashable Heading where
    hashWithSalt salt = hashWithSalt salt . convert
        where
            convert :: Heading -> Either (Maybe ()) (Maybe ())
            convert N = Left Nothing
            convert E = Left $ Just ()
            convert S = Right Nothing
            convert W = Right $ Just ()

type Position = (Coordinate, Heading)

type RawLabyrinth = Mat.Matrix RawCell
type Blueprint = Mat.Matrix Cell
type Labyrinth = (Blueprint, Coordinate, Coordinate)

type Score = Int
type Chart = HM.HashMap Position (Score, [Path])
type Path = (Score, Position, [Action])


-- Navigation utils

data Action = TurnLeft | MoveForward | TurnRight
data LR = L | R

turn :: LR -> Position -> Position
turn = second . helper
    where
        helper :: LR -> Heading -> Heading
        helper L N = W
        helper L E = N
        helper L S = E
        helper L W = S
        helper R N = E
        helper R E = S
        helper R S = W
        helper R W = N

trimap :: (a1 -> b1) -> (a2 -> b2) -> (a3 -> b3) -> (a1, a2, a3) -> (b1, b2, b3)
trimap f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)

doAction :: Action -> Path -> Path
doAction MoveForward = trimap (+ 1) moveForward (MoveForward :)
doAction turnSideways = trimap (+ 1000) (turn (case turnSideways of {TurnLeft -> L; TurnRight -> R})) (turnSideways :)

instance Show Action where
    show TurnLeft = "⮲"
    show MoveForward = "⇑"
    show TurnRight = "⮳"

moveForward :: Position -> Position
moveForward (xy, N) = (first (subtract 1) xy, N)
moveForward (xy, E) = (second (+ 1) xy, E)
moveForward (xy, S) = (first (+ 1) xy, S)
moveForward (xy, W) = (second (subtract 1) xy, W)

newtype InvertedPath = Inv Path

invertPath :: Path -> InvertedPath
invertPath = Inv . trimap id (turn L . turn L) (map invertAction)
    where
        invertAction :: Action -> Action
        invertAction TurnLeft = TurnRight
        invertAction TurnRight = TurnLeft
        invertAction MoveForward = MoveForward

collectPathCells :: Path -> [Coordinate]
collectPathCells = collect . invertPath
    where
        collect :: InvertedPath -> [Coordinate]
        collect (Inv (_, position, actions)) = fst $ foldl' helper ([fst position], position) actions
            where
                helper :: ([Coordinate], Position) -> Action -> ([Coordinate], Position)
                helper (acc, pos) MoveForward = let pos' = moveForward pos in (fst pos' : acc, pos')
                helper (acc, pos) TurnLeft = let pos' = turn L pos in (acc, pos')
                helper (acc, pos) TurnRight = let pos' = turn R pos in (acc, pos')


-- calculate single path step

type PathStep = ReaderT (Blueprint, Path) (State (Chart, [Path]))

pathStep :: PathStep (Chart, [Path])
pathStep = do
    (bp, path) <- ask
    forM_ [TurnLeft, MoveForward, TurnRight] $ \action -> do
        let path'@(score', pos', _) = doAction action path
        when (case uncurry Mat.safeGet (fst pos') bp of {(Just Empty) -> True; _ -> False}) $ do
            (hm, _) <- lift get
            case HM.lookup pos' hm of
                (Just (scoreToBeat, optimalPaths))
                    | score' > scoreToBeat -> return ()
                    | score' == scoreToBeat -> lift $ modify $ bimap (HM.insert pos' (scoreToBeat, path' : optimalPaths)) (path' :)
                _ -> lift $ modify $ bimap (HM.insert pos' (score', [path'])) (path' :)
    lift get


-- run Dijkstra's algorithm, remembering the optimal paths for each position

type Dijkstra = ReaderT Labyrinth (StateT (Chart, [Path]) (Either Chart))

dijkstra :: Dijkstra (Int, Int)
dijkstra = do
    (layout, _, end) <- ask
    flip loopM () $ \_ -> do
        initialLives <- lift $ get <&> snd
        lift $ modify (second (const []))
        forM_ initialLives $ \path -> do
            (hm, lives) <- lift get
            let (hm',newLives) = evalState (runReaderT pathStep (layout, path)) (hm, [])
            lift $ put (hm', newLives ++ lives)
        newInitialLives <- lift $ get <&> snd
        case newInitialLives of
            [] -> return $ Right ()
            _ -> return $ Left ()
    finalChart <- lift $ get <&> fst
    let chartEnd = sortOn fst $ [N, E, S, W] >>= \heading -> case HM.lookup (end, heading) finalChart of {Nothing -> empty; (Just x) -> return x}
    when (null chartEnd) $ lift $ lift $ Left finalChart
    let minScore = fst $ head chartEnd
        minPaths = concatMap snd $ filter ((minScore ==) . fst) chartEnd
        uniqueCells = nub $ concatMap collectPathCells minPaths
        numUniqueCells = length uniqueCells
    return (minScore, numUniqueCells)

evalDijkstra :: Labyrinth -> Either Chart (Int, Int)
evalDijkstra lab@(_, start, _) = evalStateT (runReaderT dijkstra lab) (HM.empty, [(0, (start, E), [])])
