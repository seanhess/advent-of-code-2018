module D01Frequency where

import Data.Maybe (mapMaybe, listToMaybe)
import Data.Set (Set)
import Debug.Trace (trace)
import qualified Data.Set as Set
import Text.Read (readMaybe)

hello :: String
hello = "Hello"


parseNumber :: String -> Maybe Integer
parseNumber ('+':cs) = readMaybe cs
parseNumber ('-':cs) = fmap negate $ readMaybe cs -- Just (readMaybe cs)
parseNumber _ = Nothing


-- how do I write a repeatedElements function
-- then take the first one



repeatedElements :: Ord a => [a] -> [a]
repeatedElements xs = foldr go (const []) xs Set.empty
  -- 'b' is a function :: Set a -> [a]
  -- this works much better than using a tuple, which doesn't evaluate as expected
  where
    go :: (Ord a) => a -> (Set a -> [a]) -> Set a -> [a]
    go x cont set
      | Set.member x set = x : cont set
      | otherwise        = cont (Set.insert x set)



firstRepeatedFrequency :: (Num a, Ord a, Show a) => [a] -> Maybe a
firstRepeatedFrequency as =
    let frequencies = scanl (+) 0 $ take 1000000 $ cycle as
        repeated = repeatedElements frequencies
    in listToMaybe repeated


program2 :: IO ()
program2 = do
    print $ firstRepeatedFrequency $ [1, -1]
    print $ firstRepeatedFrequency $ [3, 3, 4, -2, -4]
    print $ firstRepeatedFrequency $ [-6, 3, 8, 5, -6]
    print $ firstRepeatedFrequency $ [7, 7, -2, -7, -4]
    inp <- readFile "inputs/01frequency.txt"
    print $ firstRepeatedFrequency $ mapMaybe parseNumber $ lines inp


program1 :: IO ()
program1 = do
    inp <- readFile "inputs/01frequency.txt"
    let res = sum $ mapMaybe parseNumber $ lines inp
    print res


