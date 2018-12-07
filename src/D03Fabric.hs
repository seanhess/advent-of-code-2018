module D03Fabric where

-- HashMap is unordered
import Data.Char (isDigit)
import Data.List (nub, (\\))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Text.ParserCombinators.ReadP (char, ReadP, satisfy, many1, skipSpaces, string, readP_to_S, eof)


type Location = (Int, Int)
type Dimensions = (Int, Int)
type ClaimId = String

left = fst
top = snd
width = fst
height = snd

data Claim = Claim
    { claimId :: ClaimId
    , location :: Location
    , dimensions :: Dimensions
    } deriving (Show, Eq)


claimParser :: ReadP Claim
claimParser = do
    cid <- parseId
    string " @ "
    o <- parseOffset
    string ": "
    d <- parseDimensions
    eof
    return $ Claim cid o d
  where
    int = read <$> many1 digit
    digit = satisfy isDigit

    parseId = do
      char '#'
      many1 digit

    parseOffset = do
      x <- int
      char ','
      y <- int
      pure (x, y)

    parseDimensions = do
      w <- int
      char 'x'
      h <- int
      pure (w, h)


parseClaim :: String -> Maybe Claim
parseClaim s = listToMaybe $ map fst $ readP_to_S claimParser s


claimMap :: [Claim] -> HashMap Location [ClaimId]
claimMap cs = foldr addClaim HM.empty cs
  where
    addClaim :: Claim -> HashMap Location [ClaimId] -> HashMap Location [ClaimId]
    addClaim c hm =
      foldr (addLocation $ claimId c) hm $ claimLocations c

    addLocation :: ClaimId -> Location -> HashMap Location [ClaimId] -> HashMap Location [ClaimId]
    addLocation i l hm =
      HM.insertWith mergeClaim l [i] hm

    mergeClaim :: [ClaimId] -> [ClaimId] -> [ClaimId]
    mergeClaim as bs = as ++ bs


claimLocations :: Claim -> [Location]
claimLocations (Claim _ (x, y) (w, h)) =
    product [x .. x+w-1] [y .. y+h-1]
  where
      product xs ys = do
        x <- xs
        y <- ys
        pure (x, y)


conflicts :: HashMap Location [ClaimId] -> [Location]
conflicts hm = map fst $ filter isConflict $ HM.toList hm
  where isConflict (_, ids) = length ids > 1



test :: IO ()
test = do
  print $ readP_to_S claimParser "#1 @ 861,330: 20x10"
  print $ readP_to_S claimParser "#4 @ 406,769: 25x28"

  let claims = mapMaybe parseClaim sample
      fabric = claimMap claims
  print $ conflicts fabric

  let confs = conflictIds fabric
  print $ nonConflictingClaims claims confs

program :: IO ()
program = do
  inp <- readFile "inputs/03fabric.txt"
  print $ length $ conflicts $ claimMap $ mapMaybe parseClaim $ lines inp



sample =
  ["#1 @ 1,3: 4x4"
  ,"#2 @ 3,1: 4x4"
  ,"#3 @ 5,5: 2x2"
  ]


------------------------------
-- which claims don't overlap?


conflictIds :: HashMap Location [ClaimId] -> [ClaimId]
conflictIds hm = nub $ concatMap snd $ filter isConflict $ HM.toList hm
  where isConflict (_, ids) = length ids > 1

nonConflictingClaims :: [Claim] -> [ClaimId] -> [ClaimId]
nonConflictingClaims cs ids = map claimId cs \\ ids


program2 :: IO ()
program2 = do
  inp <- readFile "inputs/03fabric.txt"
  let claims = mapMaybe parseClaim $ lines inp
      fabric = claimMap claims
      confs = conflictIds fabric
  print $ nonConflictingClaims claims confs



