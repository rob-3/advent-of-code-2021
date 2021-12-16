import Data.Char
import Debug.Trace

main = do
  input <- getContents
  print $ binaryDiagnostic $ map (map digitToInt) (words input)
  print $ binaryDiagnostic2 $ map (map digitToInt) (words input)

binaryDiagnostic :: [[Int]] -> Int
binaryDiagnostic xs =
  gamma * epsilon
  where
    len = length xs
    gammaBits = map (fromEnum . \x -> x > len `quot` 2) (foldr1 (zipWith (+)) xs)
    gamma = binaryToInt gammaBits
    epsilonBits = map (fromEnum . \x -> x == 0) gammaBits
    epsilon = binaryToInt epsilonBits

findOxygenRating :: [[Int]] -> [Int] -> Int -> Int
findOxygenRating [x] bits index = binaryToInt x
findOxygenRating xs bits index = findOxygenRating remaining mostCommon (index + 1)
  where
    remaining = filter (\a -> a !! index == bits !! index) xs
    mostCommon = mostCommonBits remaining

findCo2Rating :: [[Int]] -> [Int] -> Int -> Int
findCo2Rating [x] bits index = binaryToInt x
findCo2Rating xs bits index = findCo2Rating remaining leastCommon (index + 1)
  where 
    remaining = filter (\a -> a !! index == bits !! index) xs
    leastCommon = leastCommonBits remaining

binaryDiagnostic2 :: [[Int]] -> Int
binaryDiagnostic2 xs =
  oxygenRating * co2Rating
  where
    oxygenRating = findOxygenRating xs (mostCommonBits xs) 0
    co2Rating = findCo2Rating xs (leastCommonBits xs) 0

mostCommonBits :: [[Int]] -> [Int]
mostCommonBits xs = map (fromEnum . \x -> toRational x >= toRational (length xs) / 2) (foldr1 (zipWith (+)) xs)

leastCommonBits :: [[Int]] -> [Int]
leastCommonBits xs = map (fromEnum . \x -> toRational x < toRational (length xs) / 2) (foldr1 (zipWith (+)) xs)

binaryToInt :: [Int] -> Int
binaryToInt = foldr1 (\x acc -> acc * 2 + x) . reverse
