{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import GHC.Generics (Generic)

main :: IO ()
main = do
    stones <- readFile "data/input.txt" <&> iterate blinkOnce . HM.fromList . flip zip (repeat 1) . map (S . read) . words . init
    let nStones = map sum stones
    print $ liftA2 (,) (!! 25) (!! 75) nStones

-- data structues

newtype Stone = S Integer
    deriving (Eq, Generic)

instance Hashable Stone

data StoneAfterBlink
    = Single Stone
    | Split Stone Stone

blinkStone :: Stone -> StoneAfterBlink
blinkStone (S 0) = Single (S 1)
blinkStone (S n) =
    let nLength = length $ show n
     in if even nLength
            then let (l, r) = quotRem n (10 ^ div nLength 2) in Split (S l) (S r)
            else Single (S (n * 2024))

(|>) :: a -> (a -> b) -> b
x |> f = f x

blinkOnce :: HashMap Stone Integer -> HashMap Stone Integer
blinkOnce = HM.foldlWithKey' collect HM.empty
  where
    collect :: HashMap Stone Integer -> Stone -> Integer -> HashMap Stone Integer
    collect hAcc s n = case blinkStone s of
        (Single s') -> HM.alter (justAdd n) s' hAcc
        (Split s1 s2) -> HM.alter (justAdd n) s1 hAcc |> HM.alter (justAdd n) s2
      where
        justAdd :: Integer -> Maybe Integer -> Maybe Integer
        justAdd d = Just . (+ d) . fromMaybe 0

-- correct naive algorithm with bad performance (does not terminate in reasonable amount of time for Part 2)
naive :: [()] -> Stone -> Integer
naive [] _ = 1
naive (_ : rest) (S n) =
    if n == 0
        then naive rest (S 1)
        else
            let nString = show n
             in if even $ length nString
                    then let (l, r) = splitAt (div (length nString) 2) nString in naive rest (S $ read l) + naive rest (S $ read r)
                    else naive rest (S (n * 2024))
