main = do
  n <- read <$> getLine :: IO Float
  print (n/100)
