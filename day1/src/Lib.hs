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
similarity as bs = foldl (\acc a -> acc + a * (occurrences a bs)) 0 as

occurrences :: Int -> [Int] -> Int
occurrences x xs = length (filter (== x) xs)

-- The input processing part which isn't very interesting
asIntLists :: [[String]] -> ([Int], [Int])
asIntLists pairs = foldl (\acc pair -> 
    let first     = fst acc
        second    = snd acc
        firstLoc  = read (pair !! 0) :: Int
        secondLoc = read (pair !! 1) :: Int
    in  (firstLoc : first, secondLoc : second)) ([], []) pairs

processInput :: String -> [(Int, Int)]
processInput contents =
    let pairs = map words (lines contents)
        (first, second) = asIntLists pairs
    in zip (sort first) (sort second)

-- The IO part
someFunc :: IO ()
someFunc = do
    contents <- readFile "input1.txt"
    let sortedLists     = processInput contents
        (first, second) = unzip sortedLists
    putStrLn "Distance:"
    print (distanceSum sortedLists)
    putStrLn "Similarity:"
    print (similarity first second)
