import System.IO ( stdout, hFlush )
import Data.List ( delete )
import System.Random ( getStdRandom, Random(randomR) )
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans

sortWith :: (Char -> Char -> StateT Int IO Bool) -> [Char] -> StateT Int IO [Char]
sortWith f [x] = return [x]
sortWith f [] = return []
sortWith f xs = do
  let maxN = length xs
  n <- lift . getStdRandom . randomR $ (0, maxN - 1)
  let x = xs !! n
  let xs' = delete x xs
  (ls,gs) <- partitionIO (f x) xs'
  ls' <- sortWith f ls
  gs' <- sortWith f gs
  return $ ls' ++ x : gs'
    where
      partitionIO :: (x -> StateT Int IO Bool) -> [x] -> StateT Int IO ([x], [x])
      partitionIO = partitionIO' ([], [])
        where
          partitionIO' (ts, fs) g (x:xs) = do
            b <- g x
            if b then
              partitionIO' (x:ts, fs) g xs
            else
              partitionIO' (ts, x:fs) g xs
          partitionIO' ans _ [] = return ans

question :: Char -> Char -> StateT Int IO Bool
question a b = do
  q <- get
  if q >= 1
  then do
    modify (+ (-1))
    lift . putStrLn . unwords $ ["?",[a],[b]]
    lift . hFlush $ stdout
    ans <- lift getLine
    case ans of
      ">" -> return True
      _   -> return False
  else
    return False


main :: IO ()
main = do
  [n, q] <- map read . words <$> getLine
  let str = take n ['A', 'B' ..]
  ans <- flip evalStateT q $ sortWith question str
  putStrLn . unwords $ ["!", ans]
  hFlush stdout
  return ()
