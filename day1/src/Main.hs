module Main where

import Data.List.Split (splitWhen)
import Data.List (sort)

-- Read calories from input, group by empty lines separators
readCalories :: String -> [[Int]]
readCalories = parseCalories . groupLines . lines
  where
    groupLines = splitWhen null
    parseCalories = map (map read)

-- Get calorie totals, sorted in descending order
getCalorieTotals :: [[Int]] -> [Int]
getCalorieTotals = reverse . sort . map sum

-- Get sum of top N calorie counts
topNCalorieCount :: Int -> [Int] -> Int
topNCalorieCount n = sum . take n


main :: IO ()
main = do
  calorieTotals <- getCalorieTotals . readCalories <$> getContents
  print $ topNCalorieCount 1 calorieTotals -- Part 1
  print $ topNCalorieCount 3 calorieTotals -- Part 2
