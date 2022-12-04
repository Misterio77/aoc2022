module Main where

import qualified Rucksack

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  -- Part 1
  print $ sum $ concatMap (Rucksack.priorities . Rucksack.fromString) inputLines
