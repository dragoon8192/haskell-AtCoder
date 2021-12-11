main = do
  [n, a, b] <- map read . words <$> getLine
  print . sum . filter (f a b) $ [1..n]
    where
      f a b x = a <= g x && g x <= b
      g 0 = 0
      g x = mod x 10 + g (div x 10)
