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
      | i > n || S.null abs ->  True
      | sizeIbs >  2  ->  False
      | sizeIbs == 2  ->  if
            | iIsEnd    ->  False
            | b0IsEnd   ->  calc (i+1) (IS.insert b1 . IS.delete b0 $ toEnds) abs'
            | otherwise ->  calc (i+1) toEnds $ S.insert (b0, b1) abs'
      | sizeIbs == 1  ->  if
            | iIsEnd    ->  calc (i+1) (IS.insert b0 . IS.delete i $ toEnds) abs'
            | b0IsEnd   ->  False
            | otherwise ->  calc (i+1) (IS.insert b0 toEnds) abs'
      | otherwise     -> calc (i+1) toEnds abs'
      where
        (ibs, abs') = S.split (i+1, 0) abs
        sizeIbs = S.size ibs
        (_, b0) : ib1s = S.toList ibs
        (_, b1) : _ = ib1s
        iIsEnd = IS.member i toEnds
        b0IsEnd = IS.member b0 toEnds
  putStrLn $ if calc 1 IS.empty abs
            then "Yes"
            else "No"

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
