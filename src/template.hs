import System.IO ( stdout, hFlush )
import Control.Monad ( replicateM )
import Data.Maybe ( fromJust )
import qualified Data.ByteString.Char8 as BS

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
