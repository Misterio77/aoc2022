module Rucksack (Rucksack, fromString, commonItems, priorities) where

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

newtype Rucksack = Rucksack (String, String) deriving (Show)

fromString :: String -> Rucksack
fromString x = Rucksack (splitAt ((length x + 1) `div` 2) x)

commonItems :: Rucksack -> [Char]
commonItems (Rucksack (x, y)) = dedup $ filter (`elem` x) y

priorities :: Rucksack -> [Int]
priorities = map itemPriority . commonItems
