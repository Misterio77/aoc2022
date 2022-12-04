module Main where

import Data.Char (digitToInt)
import Data.List.Split (splitOneOf)

contains :: Eq a => ([a], [a]) -> Bool
contains (range1, range2) = any (`elem` range1) range2 || any (`elem` range2) range1

fullyContains :: Eq a => ([a], [a]) -> Bool
fullyContains (range1, range2) = all (`elem` range1) range2 || all (`elem` range2) range1

rangesFromLine :: String -> ([Int], [Int])
rangesFromLine input = ([(parsed !! 0) .. (parsed !! 1)], [(parsed !! 2) .. (parsed !! 3)])
  where
    parsed = map read $ splitOneOf "-," input

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let ranges = map rangesFromLine inputLines
  -- Part 1
  print $ sum $ map (fromEnum . fullyContains) ranges
  -- Part 2
  print $ sum $ map (fromEnum . contains) ranges
