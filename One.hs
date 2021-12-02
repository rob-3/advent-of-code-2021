module One (sonarSweep) where

main = do
  input <- fmap (map read . words) getContents :: IO [Int]
  print $ sonarSweep input

sonarSweep :: [Int] -> Int
sonarSweep xs =
  let shifted = tail xs in
      sum $ map fromEnum (zipWith (>) shifted xs)
