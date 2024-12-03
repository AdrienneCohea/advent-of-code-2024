module Lib
    ( someFunc
    ) where

import Data.List (sort)

-- The pure part which represents the essence of the problem.

-- Part 1
distanceSum :: [(Int, Int)] -> Int
distanceSum pairs = foldl (\acc pair -> acc + (distance pair)) 0 pairs

distance :: (Int, Int) -> Int
distance pair = abs (fst pair - snd pair)

-- Part 2
similarity :: [Int] -> [Int] -> Int
similarity a b = foldl (\acc c -> acc + c * (occurrences c b)) 0 a

occurrences :: Int -> [Int] -> Int
occurrences x xs = length (filter (== x) xs)

-- The input processing part which isn't very interesting
asIntLists :: [[String]] -> ([Int], [Int])
asIntLists pairs = foldl getPairs ([], []) pairs

getPairs :: ([Int], [Int]) -> [String] -> ([Int], [Int])
getPairs accum pair = 
    let firstList   = fst accum
        secondList  = snd accum
        firstLocId  = read (pair !! 0) :: Int
        secondLocId = read (pair !! 1) :: Int
    in  (firstLocId : firstList, secondLocId : secondList)

processInput :: String -> [(Int, Int)]
processInput contents =
    let pairs = map words (lines contents)
        (firstList, secondList) = asIntLists pairs
    in zip (sort firstList) (sort secondList)

-- The IO part
someFunc :: IO ()
someFunc = do
    contents <- readFile "input1.txt"
    let sortedLists = processInput contents
        (first, second) = unzip sortedLists
    putStrLn "Distance:"
    print (distanceSum sortedLists)
    putStrLn "Similarity:"
    print (similarity first second)
