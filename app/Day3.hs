module Main where

import Lib
import Parser
import Text.Regex.TDFA

data Type = Mul Int Int | Do | Dont deriving (Show, Eq)

parseMul :: Parser Type
parseMul = do
	string "mul("
	x <- natural
	string ","
	y <- natural
	return $ Mul x y

parseDo :: Parser Type
parseDo = do
	string "do()"
	return Do

parseDont :: Parser Type
parseDont = do
	string "don't()"
	return Dont

parseType :: Parser Type
parseType = parseMul <|> parseDo <|> parseDont

solve1 :: [String] -> Int
solve1 xs = sum $ map (\(Mul x y) -> x * y) $ map (unsafeParse parseMul) split
	where
		ls = unlines xs
		split = getAllTextMatches (ls =~ "mul\\(([0-9])*,([0-9])*\\)") :: [String]

doStep :: Bool -> Int -> [Type] -> Int
doStep _ res [] = res
doStep shouldMult res (x:xs) = case x of
	Mul x y -> if shouldMult
		then doStep shouldMult (res + (x * y)) xs
		else doStep shouldMult res xs
	Do -> doStep True res xs
	Dont -> doStep False res xs

solve2 :: [String] -> Int
solve2 xs = doStep True 0 $ map (unsafeParse parseType) split
	where
		ls = unlines xs
		split = getAllTextMatches (ls =~ "(mul\\(([0-9])*,([0-9])*\\))|(do\\(\\))|(don't\\(\\))") :: [String]

main :: IO()
main = mainWrapper "day3" solve1 solve2
