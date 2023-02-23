module Outcome where

import Shape (Shape (Paper, Rock, Scissors), losesTo, winsFrom)

data Outcome = Win | Lose | Draw

fromChar :: Char -> Outcome
fromChar 'X' = Lose
fromChar 'Y' = Draw
fromChar 'Z' = Win
fromChar x = error $ "Invalid outcome: " ++ show x

toScore :: Outcome -> Int
toScore Win = 6
toScore Draw = 3
toScore Lose = 0

fromShapes :: Shape -> Shape -> Outcome
fromShapes opponent you
  | you == winsFrom opponent = Win
  | you == losesTo opponent = Lose
  | otherwise = Draw

-- Given opponent shape, return shape that satisfies the outcome
toShape :: Outcome -> Shape -> Shape
toShape Win x = winsFrom x
toShape Lose x = losesTo x
toShape Draw x = x
