module One (sonarSweep) where

main = do
  input <- fmap (map read . words) getContents :: IO [Int]
  print $ sonarSweep input
  print $ sonarSweep2 input

sonarSweep :: [Int] -> Int
sonarSweep xs =
  let shifted = tail xs in
      sum $ map fromEnum (zipWith (>) shifted xs)

sonarSweep2 :: [Int] -> Int
sonarSweep2 xs =
  let shifted1 = tail xs 
      shifted2 = tail shifted1
   in
      sonarSweep (zipWith3 (\a b c -> a + b + c) shifted2 shifted1 xs)
