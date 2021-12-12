main = do
  [n,x] <- map read . words <$> getLine
  as <- map read . words <$> getLine
  let ps = f as
  let aps = zip as ps
    where
      f [] = []
      f [a] = div x a + 2
      f (a0: a1: as) = div a1 a0 : f as

