module Main where

import Lib

isDecreasing :: [Int] -> Bool
isDecreasing [] = True
isDecreasing [x] = True
isDecreasing (x:y:xs)
	| x > y && (x - y) <= 3 = isDecreasing (y:xs)
	| otherwise = False

isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing [x] = True
isIncreasing (x:y:xs)
	| x < y && (y - x) <= 3 = isIncreasing (y:xs)
	| otherwise = False

isSafe :: [Int] -> Bool
isSafe xs = isDecreasing xs || isIncreasing xs

readLine :: String -> [Int]
readLine = map read . words

countSafeWithoutError :: [[Int]] -> Int
countSafeWithoutError = length . filter isSafe

solve1 :: [String] -> Int
solve1 = countSafeWithoutError . map readLine

removeElements :: [Int] -> [Int] -> [[Int]]
removeElements rs [] = [rs]
removeElements rs (x:xs) = (rs ++ xs):(removeElements (rs ++ [x]) xs)

isSafeWithError :: [Int] -> Bool
isSafeWithError xs = isSafe xs || any isSafe (removeElements[] xs)

countSafeWithError :: [[Int]] -> Int
countSafeWithError = length . filter isSafeWithError

solve2 :: [String] -> Int
solve2 = countSafeWithError . map readLine

main :: IO()
main = mainWrapper "day2" solve1 solve2
