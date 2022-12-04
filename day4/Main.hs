module Main where

import Data.Char (digitToInt)
import Data.List.Split (splitOneOf)

fullyContains :: Eq a => ([a], [a]) -> Bool
fullyContains (range1, range2) = all (`elem` range1) range2 || all (`elem` range2) range1

rangesFromLine :: String -> ([Int], [Int])
rangesFromLine input = ([(parsed !! 0) .. (parsed !! 1)], [(parsed !! 2) .. (parsed !! 3)])
  where
    parsed = map read $ splitOneOf "-," input

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  print $ sum $ map (fromEnum . fullyContains . rangesFromLine) inputLines
