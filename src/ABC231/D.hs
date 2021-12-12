import Data.List
main = do
  [n, m] <- map read . words <$> getLine :: IO [Integer]
  abs <- map (map read . words) . lines <$> getContents :: IO [[Integer]]
  let xs = permutations [1..n]
  putStrLn $ if any (allneigh abs) xs
            then "Yes"
            else "No"

neigh a b (x:y:xs) = ((a == x && b == y) || (a == y && b == x)) || neigh a b (y:xs)
neigh _ _ [x] = False
neigh _ _ [] = False

allneigh :: [[Integer]] -> [Integer] -> Bool
allneigh ([a,b]:abs) xs = if neigh a b xs
                          then allneigh abs xs
                          else False
allneigh [] _ = True
