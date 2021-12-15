{-#LANGUAGE MultiWayIf #-}
import System.IO ( stdout, hFlush )
import Control.Monad ( replicateM )
import Data.Maybe ( fromJust )
import qualified Data.ByteString.Char8 as BS
--------------------------------
-- /\ my template /\
--------------------------------
import Data.List
import qualified Data.Set as S
import qualified Data.IntSet as IS

main = do
  (n, m) <- getIntTuple
  abs <- S.fromList <$> getIntTuplesN m
  let
    calc :: Int -> IS.IntSet -> S.Set (Int, Int) -> Bool
    calc i toEnds abs = if
      | i >= n        -> True
      | sizeAibs >  2 -> False
      | sizeAibs == 2 && isEnd     ->  False
      | sizeAibs == 2 && not isEnd -> calc (i+1) toEnds $ S.insert (b0, b1) abs'
      | sizeAibs == 1 && isEnd     -> calc (i+1) (IS.insert b0 . IS.delete i $ toEnds) abs'
      | sizeAibs == 1 && not isEnd -> calc (i+1) (IS.insert b0 toEnds) abs'
      | otherwise     -> calc (i+1) toEnds abs'
      where
        (aibs, abs') = S.split (i, n) abs
        sizeAibs = S.size aibs
        (_, b0) : (_, b1) : _ = S.toList aibs
        isEnd = IS.member i toEnds
  putStrLn $ if calc 0 IS.empty abs
            then "Yes"
            else "No"

-- neigh (a, b) (x:y:xs) = ((a == x && b == y) || (a == y && b == x)) || neigh (a, b) (y:xs)
-- neigh _ [x] = False
-- neigh _ [] = False
--
-- allneigh :: [(Integer, Integer)] -> [Integer] -> Bool
-- allneigh (ab:abs) xs = neigh ab xs && allneigh abs xs
-- allneigh [] _ = True

--------------------------------
-- \/ my template \/
--------------------------------
{-#INLINE flush #-}
flush :: IO ()
flush = hFlush stdout

{-#INLINE listToTuple #-}
listToTuple :: (Integral a) => [a] -> (a,a)
listToTuple (x: y: _) = (x, y)
listToTuple xs = error $ "listToTuple error: " ++ show (map fromIntegral xs)

{-#INLINE readInt #-}
readInt :: (Integral a) => BS.ByteString -> a
readInt = fromInteger . fst . fromJust . BS.readInteger
{-#INLINE readIntTuple #-}
readIntTuple :: (Integral a) => BS.ByteString -> (a, a)
readIntTuple = listToTuple . map readInt . BS.words
{-#INLINE readIntList #-}
readIntList :: (Integral a) => BS.ByteString -> [a]
readIntList = map readInt . BS.words

{-#INLINE getBSsN #-}
getBSsN :: (Integral n) => n -> IO [BS.ByteString]
getBSsN n = take (fromIntegral n) . BS.lines <$> BS.getContents

{-#INLINE getInt #-}
getInt :: (Integral a) => IO a
getInt = readInt <$> BS.getLine
{-#INLINE getIntsN #-}
getIntsN :: (Integral a, Integral n) => n -> IO [a]
getIntsN n = map readInt <$> getBSsN n
{-#INLINE getIntsAll #-}
getIntsAll :: (Integral a) => IO [a]
getIntsAll = map readInt . BS.lines <$> BS.getContents

{-#INLINE getIntList #-}
getIntList :: (Integral a) => IO [a]
getIntList = readIntList <$> BS.getLine
{-#INLINE getIntListsN #-}
getIntListsN :: (Integral a, Integral n) => n -> IO [[a]]
getIntListsN n = map readIntList <$> getBSsN n
{-#INLINE getIntListsAll #-}
getIntListsAll :: (Integral a) => IO [[a]]
getIntListsAll = map readIntList . BS.lines <$> BS.getContents

{-#INLINE getIntTuple #-}
getIntTuple :: (Integral a) => IO (a, a)
getIntTuple = readIntTuple <$> BS.getLine
{-#INLINE getIntTuplesN #-}
getIntTuplesN :: (Integral a, Integral n) => n -> IO [(a, a)]
getIntTuplesN n = map readIntTuple <$> getBSsN n
{-#INLINE getIntTuplesAll #-}
getIntTuplesAll :: (Integral a) => IO [(a, a)]
getIntTuplesAll = map readIntTuple . BS.lines <$> BS.getContents
