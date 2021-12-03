module Day01.Part2 where

import Day01 (countAscendingWindows)
import System.Environment (getArgs)
import Util (readLines)

main :: IO ()
main = do
  depths <- getArgs >>= readLines . head
  print $ countAscendingWindows 3 depths
