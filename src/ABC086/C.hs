main = do
  n <- read <$> getLine

txys :: IO [(Int, Int, Int)]
txys = do
  [t, x, y] <- map read . words <$> getLine
  ((t, x, y): ) <$> txys
