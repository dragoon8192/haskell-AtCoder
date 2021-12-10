main = do
  [n, a, b] <- map read . words <$> getLine
  print . sum . filter (f a b) $ [1..n]
    where
      f a b x = a <= g x && g x <= b
      g = sum . map (read . (:[])) . show
