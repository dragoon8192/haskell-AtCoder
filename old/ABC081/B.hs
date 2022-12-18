main = do
  n <- getLine
  xs <- map read . words <$> getLine
  print $ f 0 xs

f n xs = if all even xs
  then f (n+1) . map (`div` 2) $ xs
  else n
