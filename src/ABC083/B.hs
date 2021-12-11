main = do
  [n, a, b] <- map read . words <$> getLine
  print . sum . filter (f a b) $ [1..n]

f a b x = a <= sumX && sumX <= b
  where
    sumX = g x
    g 0 = 0
    g x = mod x 10 + g (div x 10)
