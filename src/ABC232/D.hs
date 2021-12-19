{-#LANGUAGE MultiWayIf #-}
import System.IO ( stdout, hFlush )
import Control.Monad ( replicateM )
import Data.Maybe ( fromJust )
import qualified Data.ByteString.Char8 as BS
--------------------------------
-- /\ my template /\
--------------------------------

main = do
  (h, w) <- getIntTuple
  cij <- unlines <$> getContents :: [[Char]]
  let bij = map (map canWalk) cij
  print $ walk bij

canWalk '.' = True
canWalk '#' = False
canWalk _ = False

walk [[True]] = 1
walk [True:_]: bs = 
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
