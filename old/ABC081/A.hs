main = do
  print . sum . map (read . singleton) =<< getLine

singleton = (: [])
