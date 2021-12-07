main = do
  a <- read <$> getLine
  [b, c] <- map read . words <$> getLine
  s <- getLine
  let sum = a + b + c :: Int
  putStrLn $ show sum ++ " " ++ s

