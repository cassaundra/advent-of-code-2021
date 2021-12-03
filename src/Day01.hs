module Day01
  ( countAscending,
    countAscendingWindows,
  )
where

countAscending :: [Int] -> Int
countAscending xs = length $ filter (uncurry (<)) (zip xs (tail xs))

countAscendingWindows :: Int -> [Int] -> Int
countAscendingWindows n = countAscending . map sum . zipOffsetN n

zipOffsetN :: Int -> [a] -> [[a]]
zipOffsetN n lst
  | length lst >= n = take n lst : zipOffsetN n (tail lst)
  | otherwise = []
