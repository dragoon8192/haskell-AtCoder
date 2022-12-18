main = do
  [n, y] <- map read . words <$> getLine :: IO [Int]
  let anss = [[a,b,c] |
              a <- [0..n],
              b <- [0..n-a],
              let c = n - b - a,
              10000 * a + 5000 * b + 1000 * c == y]
  case anss of
    []  -> putStrLn . unwords . map show $ [-1, -1, -1]
    x:_ -> putStrLn . unwords . map show $ x


