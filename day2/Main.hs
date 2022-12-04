module Main where

import qualified Outcome
import qualified Shape

part1Score :: String -> Int
part1Score line =
  Outcome.toScore outcome + Shape.toScore you
  where
    opponent = Shape.fromChar $ head line
    you = Shape.fromChar $ last line
    -- Get outcome that results from yours and opponent's moves
    outcome = Outcome.fromShapes opponent you

part2Score :: String -> Int
part2Score line =
  Outcome.toScore outcome + Shape.toScore you
  where
    opponent = Shape.fromChar $ head line
    outcome = Outcome.fromChar $ last line
    -- Get move that produces outcome given opponent's move
    you = Outcome.toShape outcome opponent

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  -- Part 1
  print $ sum (map part1Score inputLines)
  -- Part 2
  print $ sum (map part2Score inputLines)
