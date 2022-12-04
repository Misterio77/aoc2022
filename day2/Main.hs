module Main where

data Shape = Rock | Paper | Scissors deriving (Eq)

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

shapeFromChar :: Char -> Shape
shapeFromChar 'A' = Rock
shapeFromChar 'B' = Paper
shapeFromChar 'C' = Scissors
shapeFromChar 'X' = Rock
shapeFromChar 'Y' = Paper
shapeFromChar 'Z' = Scissors
shapeFromChar x = error $ "Invalid shape: " ++ show x

data Outcome = Win | Lose | Draw

outcomeScore :: Outcome -> Int
outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

outcomeFromShapes :: Shape -> Shape -> Outcome
outcomeFromShapes Rock Paper = Win
outcomeFromShapes Paper Scissors = Win
outcomeFromShapes Scissors Rock = Win
outcomeFromShapes opponent you
  | opponent == you = Draw
  | otherwise = Lose

lineScore :: String -> Int
lineScore line =
  outcomeScore (outcomeFromShapes opponent you) + shapeScore you
    where
      opponent = shapeFromChar $ head line
      you = shapeFromChar $ last line

main :: IO ()
main = do
  scores <- map lineScore . lines <$> getContents
  print $ sum scores
