module D02Checksum where

import Data.List (group, sort, find, tails, sortOn)
import Data.Maybe (catMaybes, mapMaybe)

-- group them into duplicates
-- yeah, just sort them?
-- group sort etc
-- sort, then group

-- sort them into matches, then parse through tice
matches :: (Ord a, Eq a) => [a] -> [[a]]
matches = group . sort


hasNLetters :: Int -> [[a]] -> Bool
hasNLetters n xss =
    not $ null $ filter groupIsN xss
  where
    groupIsN xs = length xs == n


checksum :: [String] -> Int
checksum ids =
    let ms = map matches ids
        -- I need to count the number that match
        twos = filter (hasNLetters 2) ms
        threes = filter (hasNLetters 3) ms
    in length twos * length threes


test :: IO ()
test = do
    let inp = ["abcdef" ,"bababc" ,"abbcde" ,"abcccd" ,"aabcdd" ,"abcdee" ,"ababab" ]
    print $ checksum inp


program :: IO ()
program = do
    inp <- readFile "inputs/02checksum.txt"
    print $ checksum $ lines inp

---------------------------------
-- I Have to compare every string to every other one
-- like permutations, but only every combintation of two

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = return []
combinations n xs = do
    y:xs' <- tails xs
    ys <- combinations (n-1) xs'
    return (y:ys)

matchingElements :: Eq a => [a] -> [a] -> [a]
matchingElements as bs = catMaybes $ zipWith isMatch as bs
  where isMatch a b
          | a == b = Just a
          | otherwise = Nothing

matchingIds :: [String] -> [String]
matchingIds ids =
    reverse $ sortOn length $ map matches $ combinations 2 ids
  where
    matches [a, b] = matchingElements a b
    matches _ = []


sample2 :: [String]
sample2 = ["abcde" ,"fghij" ,"klmno" ,"pqrst" ,"fguij" ,"axcye" ,"wvxyz"]

program2 :: IO ()
program2 = do
    inp <- readFile "inputs/02checksum.txt"
    print $ head $ matchingIds $ lines inp
