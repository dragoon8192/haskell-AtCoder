import System.IO ( stdout, hFlush )
import Control.Monad ( replicateM )
import Data.Maybe ( fromJust )
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Vector as V

import Data.List

flush :: IO ()
flush = hFlush stdout

listToTuple :: (Integral a) => [a] -> (a,a)
listToTuple (x: y: _) = (x, y)
listToTuple xs = error $ "listToTuple error: " ++ show (map fromIntegral xs)

readInt :: (Integral a) => BS.ByteString -> a
readInt = fromInteger . fst . fromJust . BS.readInteger
readIntTuple :: (Integral a) => BS.ByteString -> (a, a)
readIntTuple = listToTuple . map readInt . BS.words
readIntList :: (Integral a) => BS.ByteString -> [a]
readIntList = map readInt . BS.words

getNBSs :: (Integral n) => n -> IO [BS.ByteString]
getNBSs n = replicateM (fromIntegral n) BS.getLine

getInt :: (Integral a) => IO a
getInt = readInt <$> BS.getLine
getIntsN :: (Integral a, Integral n) => n -> IO [a]
getIntsN n = map readInt <$> getNBSs n
getIntsAll :: (Integral a) => IO [a]
getIntsAll = map readInt . BS.lines <$> BS.getContents

getIntList :: (Integral a) => IO [a]
getIntList = readIntList <$> BS.getLine
getIntListsN :: (Integral a, Integral n) => n -> IO [[a]]
getIntListsN n = map readIntList <$> getNBSs n
getIntListsAll :: (Integral a) => IO [[a]]
getIntListsAll = map readIntList . BS.lines <$> BS.getContents

getIntTuple :: (Integral a) => IO (a, a)
getIntTuple = readIntTuple <$> BS.getLine
getIntTuplesN :: (Integral a, Integral n) => n -> IO [(a, a)]
getIntTuplesN n = map readIntTuple <$> getNBSs n
getIntTuplesAll :: (Integral a) => IO [(a, a)]
getIntTuplesAll = map readIntTuple . BS.lines <$> BS.getContents

filterIOBS :: (BS.ByteString -> BS.ByteString) -> IO ()
filterIOBS f = BS.putStrLn . f =<< BS.getLine
filterIOInt :: (Integral a, Show b) => (a -> b) -> IO ()
filterIOInt f = print . f =<< getInt
filterIOIntsN :: (Integral a, Integral n, Show b) => n -> (a -> b) -> IO ()
filterIOIntsN n f = do
  replicateM (fromIntegral n) . filterIOInt $ f
  return ()

main = do
  (n, q) <- getIntTuple
  as <- IM.fromListWith (+) . flip zip (repeat 1) <$> getIntList
  let gtEqNums = snd . IM.mapAccum (\acc eqNum -> (acc + eqNum, n - acc)) 0 $ as
  let gtEqCount x = maybe 0 snd . IM.lookupGE x $ gtEqNums
  xv <- V.fromList <$> getIntsN q
  -- filterIOIntsN q gtEqCount
  -- BS.putStr . BS.unlines . V.toList . V.map (BS.pack . show . gtEqCount) $ xv
  putStr . unlines . V.toList . V.map (show . gtEqCount) $ xv
