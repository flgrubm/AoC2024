{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main where

import Data.Functor ((<&>))
import Data.Matrix
import Data.Maybe (isJust, fromJust)

m1 :: Matrix Char
m1 = fromLists
    [ "....#....."
    , ".........#"
    , ".........."
    , "..#......."
    , ".......#.."
    , ".........."
    , ".#..^....."
    , "........#."
    , "#........."
    , "......#..."
    ]

main :: IO ()
main = do
    matChar <- readFile "data/input.txt" <&> fromLists . lines
    -- let matChar = m1
    let (space, guard@(GuardInfo (x, y, _))) = liftA2 (,) (fmap readCell) findGuard matChar
    -- print $ setElem (visit (Empty False False False False) UP) (4, 1) space
    space' <- runGuard space guard
    let p1 = length $ filter isVisited $ toList space'
    p2 <- fmap (length . filter id) $ cvt $ map (flip hasLoop guard . \pos -> setElem Obstacle pos space) $ filter (/= (x, y)) $ map fst $ filter (isVisited . snd) $ toList $ mapPos (,) space'
        -- map length $ map (filter (flip hasLoop guard .
    print "Result:"
    print space
    print space'
    print (p1, p2)
    -- print $ length $ filter isVisited $ toList $ runGuard space guard

cvt :: [IO a] -> IO [a]
cvt = foldr (\x acc -> do {x' <- x; acc' <- acc; pure $ x' : acc'}) (pure [])

data Direction = UP | RIGHT | DOWN | LEFT
    deriving (Show, Eq)

turn :: Direction -> Direction
turn UP = RIGHT
turn RIGHT = DOWN
turn DOWN = LEFT
turn LEFT = UP

data Cell = Obstacle
          | Empty Bool Bool Bool Bool
          | Guard Direction Bool Bool Bool Bool
          | Boom
          deriving (Eq)

isVisited :: Cell -> Bool
isVisited (Empty u r d l) = u || r || d || l
isVisited _ = False

visit :: Cell -> Direction -> (Cell, Bool)
visit (Empty u r d l) UP = (Guard UP True r d l, u)
visit (Empty u r d l) RIGHT = (Guard RIGHT u True d l, r)
visit (Empty u r d l) DOWN = (Guard DOWN u r True l, d)
visit (Empty u r d l) LEFT = (Guard LEFT u r d True, l)
visit (Guard _ u r d l) UP = (Guard UP True r d l, u)
visit (Guard _ u r d l) RIGHT = (Guard RIGHT u True d l, r)
visit (Guard _ u r d l) DOWN = (Guard DOWN u r True l, d)
visit (Guard _ u r d l) LEFT = (Guard LEFT u r d True, l)
-- visit c _ = c

unVisit :: Cell -> Cell
unVisit (Guard _ u r d l) = Empty u r d l
-- unVisit c = c

instance Show Cell where
    show Obstacle = "#"
    show e@(Empty {})
        | isVisited e = "X"
        | otherwise = "."
    show (Guard UP _ _ _ _) = "^"
    show (Guard RIGHT _ _ _ _) = ">"
    show (Guard DOWN _ _ _ _) = "v"
    show (Guard LEFT _ _ _ _) = "<"
    show Boom = "*"

newtype GuardInfo = GuardInfo (Int, Int, Direction)
    deriving (Show)

readCell :: Char -> Cell
readCell '.' = Empty False False False False
readCell '#' = Obstacle
readCell '^' = Guard UP True False False False
readCell 'X' = Empty True True True True

charIsGuard :: (Int, Int) -> Char -> Maybe (Int, Int, Direction)
charIsGuard (x, y) '^' = Just (x, y, UP)
charIsGuard (x, y) '>' = Just (x, y, RIGHT)
charIsGuard (x, y) 'v' = Just (x, y, DOWN)
charIsGuard (x, y) '<' = Just (x, y, LEFT)
charIsGuard _ _ = Nothing

findGuard :: Matrix Char -> GuardInfo
findGuard = GuardInfo . fromJust . head . filter isJust . toList . mapPos charIsGuard

nextPosition :: GuardInfo -> (Int, Int)
nextPosition (GuardInfo (x, y, UP)) = (x - 1, y)
nextPosition (GuardInfo (x, y, RIGHT)) = (x, y + 1)
nextPosition (GuardInfo (x, y, DOWN)) = (x + 1, y)
nextPosition (GuardInfo (x, y, LEFT)) = (x, y - 1)

applySnd :: (b -> c) -> (a, b) -> (a, c)
applySnd f (x, y) = (x, f y)

(|>) :: a -> (a -> b) -> b
(|>) = flip id

stepGuard :: Matrix Cell -> GuardInfo -> IO (Matrix Cell, Maybe GuardInfo, Bool)
stepGuard space guard@(GuardInfo (x, y, d)) = let
    currentCell = space ! (x, y)
    nextPos@(x', y') = nextPosition guard in
      case uncurry safeGet nextPos space of
          (Just e@(Empty {})) -> let (v, already) = visit e d
              in pure (setElem (unVisit currentCell) (x, y) space |> setElem v (x', y'), Just $ GuardInfo (x', y', d), already)
          (Just Obstacle) -> let (v, already) = visit currentCell (turn d)
              in pure (setElem v (x, y) space, Just $ GuardInfo (x, y, turn d), already)
          Nothing -> pure (setElem (unVisit currentCell) (x, y) space, Nothing, False) -- guard left the map

runGuard :: Matrix Cell -> GuardInfo -> IO (Matrix Cell)
runGuard space guard = do
    res <- stepGuard space guard
    case res of
      (space', Just guard', False) -> runGuard space' guard'
      (space', Just (GuardInfo (x, y, _)), True) -> pure $ setElem (unVisit (space' ! (x, y))) (x, y) space'
      (space', _, _) -> pure space'

hasLoop :: Matrix Cell -> GuardInfo -> IO Bool
hasLoop space guard = do
    res <- stepGuard space guard
    case res of
      (space', Just guard', False) -> hasLoop space' guard'
      (_, Nothing, False) -> pure False
      (_, _, _) -> pure True
