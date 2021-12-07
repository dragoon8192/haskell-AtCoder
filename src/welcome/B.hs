import System.IO ( stdout, hFlush )
import Data.List ( delete )
import System.Random ( getStdRandom, Random(randomR) )

sortWith :: (Char -> Char -> IO Bool) -> [Char] -> IO [Char]
sortWith f [x] = return [x]
sortWith f [] = return []
sortWith f xs = do
  let maxN = length xs
  n <- getStdRandom . randomR $ (0, maxN)
  let x = xs !! n
  let xs' = delete x xs
  (ls,gs) <- partitionIO (f x) xs
  ls' <- sortWith f ls
  gs' <- sortWith f gs
  return $ ls' ++ x : gs'
    where
      partitionIO :: (x -> IO Bool) -> [x] -> IO ([x], [x])
      partitionIO = partitionIO' ([], [])
        where
          partitionIO' (ts, fs) g (x:xs) = do
            b <- g x
            if b then
              partitionIO' (x:ts, fs) g xs
            else
              partitionIO' (ts, x:fs) g xs
          partitionIO' ans _ [] = return ans

question :: Char -> Char -> IO Bool
question a b = do
  putStrLn . unwords $ ["?",[a],[b]]
  hFlush stdout
  ans <- getLine
  case ans of
    ">" -> return True
    _   -> return False

main :: IO ()
main = do
  [n, q] <- map read . words <$> getLine
  let str = take n ['A', 'B' ..]
  ans <- sortWith question str
  putStrLn . unwords $ ["!", ans]
  hFlush stdout
  return ()
