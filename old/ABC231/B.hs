import Data.List
import Data.Function
main = do
  n <- getLine
  names <- sort . lines <$> getContents
  putStrLn . fst . maximumBy (compare `on` snd) . f [] $ names

f nms [] = nms
f [] (x:xs) = f [(x,1)] xs
f nms'@((name,m):nms) (x:xs) = if name == x
  then f ((name, m+1):nms) xs
  else f ((x,1):nms') xs
