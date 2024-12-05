{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main where

import Data.Functor ((<&>))
import Data.List (sortOn)

main :: IO ()
main = do
    orders <- readFile "data/orderings.txt" <&> map ((\[x, y] -> (x, y)) . map (read :: String -> Int) . foldr (split '|') []) . lines
    updates <- readFile "data/updates.txt" <&> map (map (read :: String -> Int) . foldr (split ',') []) . lines
    print $ applyToBoth (sum . map getMiddleElementOddList) $ applySeparately id (map (sortOn (Update orders))) $ liftA2 (,) (filter (lineIsOkay orders)) (filter (not . lineIsOkay orders)) updates

applyToBoth :: (a -> b) -> (a, a) -> (b, b)
applyToBoth f (x, y) = (f x, f y)

applySeparately :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
applySeparately f g (x, y) = (f x, g y)

split :: Char -> Char -> [String] -> [String]
split splitter current lst
    | splitter == current = [] : lst
    | otherwise = case lst of
        [] -> [[current]]
        (h : rest) -> (current : h) : rest

precedes :: [(Int, Int)] -> Int -> Int -> Bool
precedes ord x y = any (\a -> a == (y, x)) ord

data PageUpdate = Update [(Int, Int)] Int
    deriving (Eq)

instance Ord PageUpdate where
    (<=) (Update ord m) (Update _ n) = precedes ord n m

lineIsOkay :: [(Int, Int)] -> [Int] -> Bool
lineIsOkay _ [] = True
lineIsOkay ord (h : rest) = not (any (precedes ord h) rest) && lineIsOkay ord rest

getMiddleElementOddList :: [a] -> a
getMiddleElementOddList lst = lst !! div (length lst - 1) 2
