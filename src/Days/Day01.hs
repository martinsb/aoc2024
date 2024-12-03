module Days.Day01 (part1, part2) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List

parseLine :: String -> (Int, Int)
parseLine line =
    let [n1, n2] = map read $ words line
    in (n1, n2)

distance :: Int -> Int -> Int
distance n1 n2 = abs (n1 - n2)

totalDistance :: [Int] -> [Int] -> Int
totalDistance col1 col2 = sum $ zipWith distance col1 col2

part1 :: String -> IO Int
part1 inputFile = do
    input <- readFile inputFile
    let (rawCol1, rawCol2) = foldr (\(n1, n2) (col1, col2) -> (n1 : col1, n2 : col2)) ([], []) $ map parseLine $ lines input
    let sortedCol1 = sort rawCol1
    let sortedCol2 = sort rawCol2
    return $ totalDistance sortedCol1 sortedCol2

numberOccurrences :: [Int] -> Int -> Int
numberOccurrences [] _ = 0
numberOccurrences (x:xs) n = if x == n then 1 + numberOccurrences xs n else numberOccurrences xs n

part2 :: String -> IO Int
part2 inputFile = do
    input <- readFile inputFile
    let (left, right) = foldr (\(n1, n2) (col1, col2) -> (n1 : col1, n2 : col2)) ([], []) $ map parseLine $ lines input
    let totalNumbers = foldr (\x acc -> acc + x * numberOccurrences right x) 0 left
    return totalNumbers