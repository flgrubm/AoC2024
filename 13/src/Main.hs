{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main where

import Control.Monad (join)
import Data.Bifunctor (bimap, first, second)
import Data.Functor ((<&>))
import Data.Matrix
import Data.Maybe
import Data.Ratio

main :: IO ()
main = do
    clawMachines <- readFile "data/input.txt" <&> parseClawMachines . map words . lines
    print $ both (sum . map (uncurry (+) . first (* 3)) . mapMaybe solveClawMachine) $ second (map (adjustError 10000000000000)) $ join (,) clawMachines

-- helper functions
both :: (a -> b) -> (a, a) -> (b, b)
both = join bimap

rationalToInt :: Rational -> Maybe Integer
rationalToInt r =
    let (n, d) = (numerator r, denominator r)
        (q, s) = quotRem n d
     in if s == 0 then Just q else Nothing

newtype ClawMachine = M ((Integer, Integer), (Integer, Integer), (Integer, Integer))
    deriving (Show)

d2R :: String -> Integer
d2R = read . drop 2

-- all parameters of a claw machine have to be positive
parseClawMachines :: [[String]] -> [ClawMachine]
parseClawMachines ([] : rest) = parseClawMachines rest
parseClawMachines ([_, _, aX, aY] : [_, _, bX, bY] : [_, tX, tY] : rest) = M ((d2R $ init aX, d2R aY), (d2R $ init bX, d2R bY), (d2R (init tX), d2R tY)) : parseClawMachines rest
parseClawMachines _ = []

-- maybe return an optimal solution to the given claw machine instance
solveClawMachine :: ClawMachine -> Maybe (Integer, Integer)
solveClawMachine (M ((a1, a2), (b1, b2), (t1, t2))) = case inverse (fromLists [[a1 % 1, b1 % 1], [a2 % 1, b2 % 1]]) of
    (Right m) ->
        let [cA, cB] = toList $ m `multStd` fromLists [[t1 % 1], [t2 % 1]]
         in if cA < 0 || cB < 0
                then Nothing
                else do
                    c1 <- rationalToInt cA
                    c2 <- rationalToInt cB
                    pure (c1, c2)
    -- the next case is not present in the input data
    _ -> case ((quotRem t1 a1, quotRem t2 a2), (quotRem t1 b1, quotRem t2 b2)) of
        (((qA1, 0), (qA2, 0)), ((qB1, 0), (qB2, 0)))
            | qA1 >= 0 && qB1 >= 0 && qA1 == qA2 && qB1 == qB2 -> if qB1 <= 3 * qA1 then Just (0, qB1) else Just (qA1, 0)
        (((qA1, 0), (qA2, 0)), _)
            | qA1 >= 0 && qA1 == qA2 -> Just (qA1, 0)
        (_, ((qB1, 0), (qB2, 0)))
            | qB1 >= 0 && qB1 == qB2 -> Just (0, qB1)
        _ -> Nothing

-- for Part 2
adjustError :: Integer -> ClawMachine -> ClawMachine
adjustError corr (M ((a1, a2), (b1, b2), (t1, t2))) = M ((a1, a2), (b1, b2), (t1 + corr, t2 + corr))
