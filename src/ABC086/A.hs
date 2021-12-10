main = do
  putStrLn . prodEven . map readInt . words =<< readLn

prodEven xs = if any even xs then "Even" else "Odd"

readInt :: String -> Int
readInt = read
