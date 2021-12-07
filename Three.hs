import Data.Char

main = do
  input <- getContents
  print $ binaryDiagnostic $ map (map digitToInt) (words input)

binaryDiagnostic :: [[Int]] -> Integer
binaryDiagnostic xs =
  binaryToInt gamma * binaryToInt epsilon
  where
    len = length xs
    gamma = map (toInteger . fromEnum . \x -> x > len `quot` 2) (foldr1 (zipWith (+)) xs)
    epsilon = map (toInteger . fromEnum . \x -> x == 0) gamma

binaryToInt :: [Integer] -> Integer
binaryToInt = foldr1 (\x acc -> acc * 2 + x) . reverse
