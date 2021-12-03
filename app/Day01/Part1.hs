module Day01.Part1 where

import Day01 (countAscending)
import System.Environment (getArgs)
import Util (readLines)

main :: IO ()
main = do
  depths <- getArgs >>= readLines . head
  print $ countAscending depths
