module Main where

import qualified Outcome
import qualified Shape

lineScore :: String -> Int
lineScore line = Outcome.toScore outcome + Shape.toScore you
  where
    outcome = Outcome.fromShapes opponent you
    opponent = Shape.fromChar $ head line
    you = Shape.fromChar $ last line

main :: IO ()
main = do
  scores <- map lineScore . lines <$> getContents
  print $ sum scores
