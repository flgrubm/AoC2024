{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main where

import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List (foldl')

main :: IO ()
main = do
    files <- readFile "data/input.txt" <&> collectFiles . init
    let p1 = checksum $ defrag tryDefragOnce1 files
        p2 = checksum $ defrag tryDefragOnce2 files
    print (p1, p2)

-- reading files from string encoding

newtype File = F (Int, Int, Int) -- (ID, number of blocks, number of subsequent empty blocks)

collectFiles :: String -> [File]
collectFiles = fixLast . foldl' collect ([], 0, Nothing)
  where
    collect :: ([File], Int, Maybe Int) -> Char -> ([File], Int, Maybe Int)
    collect (acc, currentID, Nothing) c = (acc, currentID, Just (digitToInt c))
    collect (acc, currentID, Just nBlocks) c = (F (currentID, nBlocks, digitToInt c) : acc, currentID + 1, Nothing)

    fixLast :: ([File], Int, Maybe Int) -> [File]
    fixLast (lst, _, Nothing) = reverse lst
    fixLast (lst, currentID, Just nBlocks) = reverse $ F (currentID, nBlocks, 0) : lst

-- defragmenting

-- Part 1
insertSpread1 :: ([File], File, Int) -> File -> ([File], File, Int)
insertSpread1 (acc, f@(F (iD, _, _)), nBlocksLeft) f'@(F (iD', nBlocks', nFree'))
    | nFree' < 1 || nBlocksLeft < 1 = (f' : acc, f, nBlocksLeft)
    | otherwise =
        if nFree' > nBlocksLeft
            then (F (iD, nBlocksLeft, nFree' - nBlocksLeft) : F (iD', nBlocks', 0) : acc, f, 0)
            else (F (iD, nFree', 0) : F (iD', nBlocks', 0) : acc, f, nBlocksLeft - nFree')

tryDefragOnce1 :: Int -> [File] -> [File]
tryDefragOnce1 iD fs =
    let (pre, post) = break (\(F (iD', _, _)) -> iD == iD') fs
     in case (pre, post) of
            (_ : _, f@(F (_, nBlocks, nFree)) : rest) ->
                let (F (iD', nBlocks', nFree') : lst, _, nBlocksLeft) = foldl' insertSpread1 ([], f, nBlocks) pre
                 in if nBlocksLeft < 1
                        then reverse lst ++ (F (iD', nBlocks', nFree' + nBlocks + nFree) : rest)
                        else
                            if iD' /= iD
                                then reverse lst ++ (F (iD', nBlocks', 0) : F (iD, nBlocksLeft, nFree' + nBlocks + nFree - nBlocksLeft) : rest)
                                else reverse lst ++ (F (iD, nBlocks + nBlocks', nFree' + nBlocks + nFree - nBlocksLeft) : rest)
            ([], _) -> post -- iD is the first to occur
            (_, []) -> pre -- iD doesn't occur

-- Part 2
insertSpread2 :: ([File], File, Int) -> File -> ([File], File, Int)
insertSpread2 (acc, f@(F (iD, _, _)), nBlocksLeft) f'@(F (iD', nBlocks', nFree'))
    | nFree' < nBlocksLeft || nBlocksLeft < 1 = (f' : acc, f, nBlocksLeft)
    | otherwise = (F (iD, nBlocksLeft, nFree' - nBlocksLeft) : F (iD', nBlocks', 0) : acc, f, 0)

tryDefragOnce2 :: Int -> [File] -> [File]
tryDefragOnce2 iD fs =
    let (pre, post) = break (\(F (iD', _, _)) -> iD == iD') fs
     in case (pre, post) of
            (_ : _, f@(F (_, nBlocks, nFree)) : rest) ->
                let (F (iD', nBlocks', nFree') : lst, _, nBlocksLeft) = foldl' insertSpread2 ([], f, nBlocks) pre
                 in if nBlocksLeft < 1
                        then reverse lst ++ (F (iD', nBlocks', nFree' + nBlocks + nFree) : rest)
                        else fs
            ([], _) -> post
            (_, []) -> pre

-- defragment with the specified defragmentation function
defrag :: (Int -> [File] -> [File]) -> [File] -> [File]
defrag tDO fs = defrag' tDO (reverse [1 .. (length fs)] :: [Int]) fs
  where
    defrag' :: (Int -> [File] -> [File]) -> [Int] -> [File] -> [File]
    defrag' tryDefragOnce (iD : iDs) fs = defrag' tryDefragOnce iDs $ tryDefragOnce iD fs
    defrag' _ [] fs = fs

checksum :: [File] -> Int
checksum = fst . foldl' collect (0, 0)
  where
    collect :: (Int, Int) -> File -> (Int, Int)
    collect (acc, n) (F (iD, nBlocks, nFree)) = (acc + sum (map (* iD) [n .. (n + nBlocks - 1)]), n + nBlocks + nFree)

-- printing
newtype Block = B (Maybe Int)

instance Show Block where
    show (B (Just i)) = show i
    show (B Nothing) = "."

getBlocks :: [File] -> [Block]
getBlocks = reverse . foldl' collect [] -- it might be slightly more efficient to add the short lists in the front and then reverse once in the end
  where
    collect :: [Block] -> File -> [Block]
    collect acc (F (iD, nBlocks, nFree)) = replicate nFree (B Nothing) ++ replicate nBlocks (B (Just iD)) ++ acc

showLayoutFromBlocks :: [Block] -> String
showLayoutFromBlocks = concatMap show

showLayoutFromFiles :: [File] -> String
showLayoutFromFiles = showLayoutFromBlocks . getBlocks
