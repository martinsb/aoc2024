module Days.Day01 (part1, part2) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List

parseLine :: String -> [Int]
parseLine line = map read $ words line

data Direction = Unknown | Ascending | Descending

diffIsSafe :: Int -> Int -> Bool
diffIsSafe x y = diff >= 1 && diff <= 3
    where diff = abs $ x - y

reportIsSafe :: [Int] -> Direction -> Int -> Bool
reportIsSafe [] _ _ = True
reportIsSafe [x] Unknown _ = False
reportIsSafe [x] Ascending prev = x > prev && diffIsSafe x prev
reportIsSafe [x] Descending prev = x < prev && diffIsSafe x prev
reportIsSafe (x:xs) Ascending prev = if x > prev && diffIsSafe x prev then reportIsSafe xs Ascending x else False
reportIsSafe (x:xs) Descending prev = if x < prev && diffIsSafe x prev then reportIsSafe xs Descending x else False
reportIsSafe (x:y:xs) Unknown 0 = reportIsSafe (y:xs) (if x < y then Ascending else Descending) x

part1 :: String -> IO Int
part1 inputFile = do
    input <- readFile inputFile
    let reports = map parseLine $ lines input
    let safeReports = filter (\report -> reportIsSafe report Unknown 0) reports
    return $ length safeReports

reportIsFixable :: [Int] -> Direction -> Int -> Bool
reportIsFixable report direction prev = if reportIsSafe report direction prev then True else
    length (filter (\i -> reportIsSafe (removeAt i report) Unknown 0) indices) > 0
        where indices = [0..(length report - 1)]

removeAt :: Int -> [a] -> [a]
removeAt index xs = [x | (x, i) <- zip xs [0..], i /= index]


part2 :: String -> IO Int
part2 inputFile = do
    input <- readFile inputFile
    let reports = map parseLine $ lines input
    let safeReports = filter (\report -> reportIsFixable report Unknown 0) reports
    return $ length safeReports