import One

main = do
  input <- fmap (map read . words) getContents :: IO [Int]
  print $ sonarSweep2 input

sonarSweep2 :: [Int] -> Int
sonarSweep2 xs =
  let shifted1 = tail xs 
      shifted2 = tail shifted1
   in
      sonarSweep (zipWith3 (\a b c -> a + b + c) shifted2 shifted1 xs)
