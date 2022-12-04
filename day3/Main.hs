module Main where

import Data.List.Split (chunksOf)
import qualified Rucksack

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let rucksacks = map Rucksack.fromString inputLines
  -- Part 1
  let priorities = concatMap Rucksack.getPriorities rucksacks
  print $ sum priorities
  -- Part 2
  let badges = map Rucksack.getBadge $ chunksOf 3 rucksacks
  let priorities = map Rucksack.itemPriority badges
  print $ sum priorities
