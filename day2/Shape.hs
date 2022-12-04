module Shape where

data Shape = Rock | Paper | Scissors deriving (Eq)

toScore :: Shape -> Int
toScore Rock = 1
toScore Paper = 2
toScore Scissors = 3

fromChar :: Char -> Shape
fromChar 'A' = Rock
fromChar 'B' = Paper
fromChar 'C' = Scissors
fromChar 'X' = Rock
fromChar 'Y' = Paper
fromChar 'Z' = Scissors
fromChar x = error $ "Invalid shape: " ++ show x
