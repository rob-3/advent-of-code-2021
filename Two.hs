main = do
  input <- getContents
  print $ dive $ toTuples $ words input

dive :: [(String, Int)] -> Int
dive pairs = let (x, y) = foldl addPairs (0, 0) (map convert pairs) in  x * y

addPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

toTuples :: [String] -> [(String, Int)]
toTuples (x : y : xs) = (x, read y) : toTuples xs
toTuples [] = []
toTuples _ = error "bad input"

convert :: (String, Int) -> (Int, Int)
convert ("forward", n) = (n, 0)
convert ("down", n) = (0, n)
convert ("up", n) = (0, - n)
convert _ = error "bad input"
