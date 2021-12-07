main = do
  input <- getContents
  let tuples = toTuples $ words input
  print $ dive tuples
  print $ diveB tuples

dive :: [(String, Int)] -> Int
dive pairs = let (x, y) = foldl addPairs (0, 0) (map convert pairs) in x * y

diveB :: [(String, Int)] -> Int
diveB pairs = let (x, y, _) = foldl addPairsB (0, 0, 0) (map convertB pairs) in x * y

addPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

addPairsB :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addPairsB (x1, y1, aim1) (x2, y2, aim2) = (x1 + x2, y1 + y2 * aim1, aim1 + aim2)

toTuples :: [String] -> [(String, Int)]
toTuples (x : y : xs) = (x, read y) : toTuples xs
toTuples [] = []
toTuples _ = error "bad input"

convert :: (String, Int) -> (Int, Int)
convert ("forward", n) = (n, 0)
convert ("down", n) = (0, n)
convert ("up", n) = (0, - n)
convert _ = error "bad input"

convertB :: (String, Int) -> (Int, Int, Int)
convertB ("forward", n) = (n, n, 0)
convertB ("down", n) = (0, 0, n)
convertB ("up", n) = (0, 0, - n)
convertB _ = error "bad input"
