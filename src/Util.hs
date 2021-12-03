module Util where

readLines :: Read a => FilePath -> IO [a]
readLines path = do
  content <- readFile path
  return $ map read $ lines content
