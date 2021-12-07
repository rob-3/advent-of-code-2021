import Data.Char

main = do
  input <- getContents
  print $ binaryDiagnostic $ map (map digitToInt) (words input)

binaryDiagnostic :: [[Int]] -> Integer
binaryDiagnostic xs =
  gamma * epsilon
  where
    len = length xs
    gammaBits = map (toInteger . fromEnum . \x -> x > len `quot` 2) (foldr1 (zipWith (+)) xs)
    gamma = binaryToInt gammaBits
    epsilonBits = map (toInteger . fromEnum . \x -> x == 0) gammaBits
    epsilon = binaryToInt epsilonBits

binaryToInt :: [Integer] -> Integer
binaryToInt = foldr1 (\x acc -> acc * 2 + x) . reverse
