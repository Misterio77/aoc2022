module Main where

import Data.Char (digitToInt)
import Data.List.Split (splitOneOf)

-- Check whether the lists intercept
intersects :: Eq a => ([a], [a]) -> Bool
intersects (range1, range2) = any (`elem` range1) range2

-- Check whether one of the lists fully contains the other
contains :: Eq a => ([a], [a]) -> Bool
contains (range1, range2) = all (`elem` range1) range2 || all (`elem` range2) range1

rangesFromLine :: String -> ([Int], [Int])
rangesFromLine input = (range1, range2)
  where
    range1 = [(parsed !! 0) .. (parsed !! 1)]
    range2 = [(parsed !! 2) .. (parsed !! 3)]
    parsed = map read $ splitOneOf "-," input

countTrues :: [Bool] -> Int
countTrues = sum . map fromEnum

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let ranges = map rangesFromLine inputLines
  -- Part 1
  print $ countTrues $ map contains ranges
  -- Part 2
  print $ countTrues $ map intersects ranges
