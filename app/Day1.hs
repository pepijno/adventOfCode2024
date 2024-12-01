module Main where

import Lib
import Data.List

splitLine :: String -> (Int, Int)
splitLine s = (read (ws !! 0), read (ws !! 1))
	where
		ws = words s

parseInput :: [String] -> ([Int], [Int])
parseInput xs = (sort a, sort b)
	where
		(a, b) = unzip $ map splitLine xs

compareLists :: ([Int], [Int]) -> [Int]
compareLists ([], []) = []
compareLists (x:xs, y:ys) = (abs(x - y)):compareLists (xs, ys)

solve1 :: [String] -> Int
solve1 = sum . compareLists . parseInput

countInOther :: Int -> [Int] -> Int
countInOther x xs = length $ filter (==x) xs

solve2 :: [String] -> Int
solve2 xs = sum $ map (\x -> x * (countInOther x b)) a
	where
		(a, b) = unzip $ map splitLine xs

main :: IO()
main = mainWrapper "day1" solve1 solve2
