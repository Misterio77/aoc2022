module Main where

import Data.Char (isLower, isUpper, ord)
import Data.List (group)
import Data.List.Split (chunksOf)
import GHC.List (splitAt)

dedup :: (Eq a) => [a] -> [a]
dedup = map head . group

type Sack = String

type Item = Char

-- Get item priority value
itemPriority :: Item -> Int
itemPriority x
  | isUpper x = ord x - 38
  | isLower x = ord x - 96
  | otherwise = error ("Invalid item: " ++ show x)

-- Given group of items, get total priority sum
totalPriority :: [Item] -> Int
totalPriority = sum . map itemPriority

-- Get common items (items present in both halves of the given sack)
inBothPockets :: Sack -> [Item]
inBothPockets sack = dedup $ filter (`elem` pocket1) pocket2
  where
    middle = length sack `div` 2
    (pocket1, pocket2) = splitAt middle sack

-- Get badge items (items present in all of the given sacks)
getBadges :: [Sack] -> [Item]
getBadges [] = [] -- No sacks, no badges
getBadges (firstSack : otherSacks) = dedup $ filter isInAllSacks firstSack
  where
    isInAllSacks i = all (i `elem`) otherSacks

main :: IO ()
main = do
  sacks <- lines <$> getContents
  -- Part 1
  let commonItems = concatMap inBothPockets sacks
  print $ totalPriority commonItems
  -- Part 2
  let badgeItems = concatMap getBadges $ chunksOf 3 sacks
  print $ totalPriority badgeItems
