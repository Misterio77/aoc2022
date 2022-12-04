module Rucksack where

import Data.Char (isLower, isUpper, ord)
import Data.List (group)
import GHC.List (splitAt)

dedup :: (Eq a) => [a] -> [a]
dedup = map head . group

itemPriority :: Char -> Int
itemPriority x
  | isUpper x = ord x - 38
  | isLower x = ord x - 96
  | otherwise = error ("Invalid item: " ++ show x)

newtype Rucksack = Rucksack String

fromString :: String -> Rucksack
fromString = Rucksack

hasItem :: Rucksack -> Char -> Bool
hasItem sack item = item `elem` getItems sack

getItems :: Rucksack -> String
getItems (Rucksack items) = items

getPockets :: Rucksack -> (String, String)
getPockets (Rucksack items) = splitAt ((length items + 1) `div` 2) items

inBothPockets :: Rucksack -> [Char]
inBothPockets sack = dedup $ filter (`elem` a) b
  where
    (a, b) = getPockets sack

getPriorities :: Rucksack -> [Int]
getPriorities = map itemPriority . inBothPockets

getBadge :: [Rucksack] -> Char
getBadge sacks = head $ filter isInAllSacks firstSackItems
  where
    firstSackItems = getItems $ head sacks
    isInAllSacks i = all (`hasItem` i) sacks
