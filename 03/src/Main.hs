{-
Copyright © 2024 Fabian Grubmüller

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (foldl', sortOn, tails)
import Data.Ord
import Data.Char

main :: IO ()
main =
    readFile "data/input.txt"
        >>= print . liftA2 (,) f1 f2

data Token = Unmatched
    | Mul
    | LParen
    | RParen
    | Numeric Int
    | Comma
    | Do
    | Dont
    deriving (Show, Eq)

keywords :: [(Token, String)]
keywords = sortOn (Data.Ord.Down . length . snd)
    [ (Mul, "mul")
    , (LParen, "(")
    , (RParen, ")")
    , (Comma, ",")
    , (Do, "do()")
    , (Dont, "don't()")
    ]

-- filter the keywords that can still match after reading the current character
filterKeywords :: Char -> [(Token, String)] -> [(Token, String)]
filterKeywords c lst = [(tok, rest) | (tok, h : rest) <- lst, h == c]

-- if possible, match the longest keyword at the current location
tryMatchKeywords :: String -> [(Token, String)] -> Int -> Maybe (Token, Int)
tryMatchKeywords [] _ _ = Nothing
tryMatchKeywords (c : rest) lst n = case filterKeywords c lst of
    [] -> Nothing
    [h] -> Just (fst h, n + 1 + length (snd h))
    rlst -> tryMatchKeywords rest rlst (n + 1)

-- if possible, match the longest integer token at the current location
tryMatchNumeric :: String -> Int -> Int -> Maybe (Token, Int)
tryMatchNumeric [] subtotal n
    | n <= 0 = Nothing
    | otherwise = Just (Numeric subtotal, n)
tryMatchNumeric (c : rest) subtotal n = if isDigit c
    then tryMatchNumeric rest (subtotal * 10 + digitToInt c) (n + 1)
    else tryMatchNumeric "" subtotal n

-- try to match a token at the current location
tryMatch :: String -> (Token, Int)
tryMatch str = case (tryMatchKeywords str keywords 0, tryMatchNumeric str 0 0) of
    (Just kw, _) -> kw
    (_, Just num) -> num
    _ -> (Unmatched, if length str == 0 then 0 else 1)

-- collect the longest matching tokens and collapse subsequent unmatched tokens into one
collectTokens :: ([Token], Int) -> (Token, Int) -> ([Token], Int)
collectTokens (lst, skip) (t, len)
    | skip <= 0 = case (lst, t) of
        (Unmatched : _, Unmatched) -> (lst, len - 1)
        _ -> (t : lst, len - 1)
    | otherwise = (lst, skip - 1)

data Operation = Prod Int Int
    | EnableProd
    | DisableProd
    deriving (Show, Eq)

translateTokens :: [Token] -> [Operation]
translateTokens (Mul : LParen : (Numeric m) : Comma : (Numeric n) : RParen : rest) = (Prod m n) : translateTokens rest
translateTokens (Do : rest) = EnableProd : translateTokens rest
translateTokens (Dont : rest) = DisableProd : translateTokens rest
translateTokens [] = []
translateTokens (_ : rest) = translateTokens rest

parse:: String -> [Operation]
parse = translateTokens .reverse . tail . fst . foldl' collectTokens ([], 0) . map tryMatch . tails

f :: ((Int, Bool) -> Operation -> (Int, Bool)) -> String -> Int
f g = fst . foldl' g (0, True) . parse

-- Part 1
f1Helper :: (Int, Bool) -> Operation -> (Int, Bool)
f1Helper (subtotal, b) (Prod m n) = ((m * n) + subtotal, b)
f1Helper acc _ = acc

f1 :: String -> Int
f1 = f f1Helper

-- Part 2
f2Helper :: (Int, Bool) -> Operation -> (Int, Bool)
f2Helper (subtotal, _) EnableProd = (subtotal, True)
f2Helper (subtotal, _) DisableProd = (subtotal, False)
f2Helper (subtotal, True) (Prod m n) = ((m * n) + subtotal, True)
f2Helper (subtotal, False) _ = (subtotal, False)

f2 :: String -> Int
f2 = f f2Helper
