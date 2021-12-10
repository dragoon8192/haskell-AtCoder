main = do
  [a,b,c,n] <- map read . lines <$> getContents
  let anss = [(a', b', c') | a' <- [0..a], b' <- [0..b], c' <- [0..c], 500 * a' + 100 * b' + 50 * c' == n]
  print . length $ anss
