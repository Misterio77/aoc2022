module Outcome where

import Shape (Shape (Paper, Rock, Scissors))

data Outcome = Win | Lose | Draw

toScore :: Outcome -> Int
toScore Win = 6
toScore Draw = 3
toScore Lose = 0

fromShapes :: Shape -> Shape -> Outcome
fromShapes Rock Paper = Win
fromShapes Paper Scissors = Win
fromShapes Scissors Rock = Win
fromShapes opponent you
  | opponent == you = Draw
  | otherwise = Lose
