import Data.List
main = do
  getLine
  xs <- map read . words <$> getLine :: IO [Int]
  print . abs . foldr (-) 0 . sort $ xs
