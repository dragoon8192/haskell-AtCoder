main = do
  putStrLn . prodEven . map read . words =<< readLn

prodEven xs = if any even xs then "Even" else "Odd"
