{-#LANGUAGE MultiWayIf #-}
import System.IO ( stdout, hFlush )
import Control.Monad ( replicateM )
import Data.Maybe ( fromJust )
import qualified Data.ByteString.Char8 as BS
--------------------------------
-- /\ my template /\
--------------------------------
import Data.List
import Control.Arrow
import qualified Data.Set as S
import qualified Data.IntSet as IS

data NodeOrEnd = Node Int | End Int
  deriving (Eq, Ord)
fromNode (Node a) = a
fromNode _ = undefined

main = do
  (n, m) <- getIntTuple
  abs <- S.fromList . map (second Node) <$> getIntTuplesN m
  let
  putStrLn $ if calc 1 abs
            then "Yes"
            else "No"

calc :: Int -> S.Set (Int, NodeOrEnd) -> Bool
calc i abs = if S.null abs
  then True
  else case map snd . S.toList $ ibs of
    [Node b0]              -> insCalc (b0, End i) (i+1) abs'
    [Node b0, nb1]         -> loopCheckInsCalc (b0, nb1) (i+1) abs'
    []                     -> calc (i+1) abs'
    [End b0]               -> calc (i+1) abs'
    [End b0, _]            -> calc (i+1) abs'
    _                      -> False
  where
    (ibs, abs') = S.spanAntitone ((i ==) . fst) abs
    loopCheckInsCalc ab i abs = if S.member ab abs
      then  False
      else insCalc ab i abs
    insCalc ab i abs = calc i $ S.insert ab abs

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
