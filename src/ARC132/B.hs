{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf #-}

import Prelude hiding (sum, product)
import Data.List hiding (sum, product)
import System.IO ( stdout, hFlush )
import Control.Monad ( replicateM )
import Data.Maybe ( fromJust )
import qualified Data.ByteString.Char8 as BS

--------------------------------
-- /\ my template /\
--------------------------------

main = do
  n <- getInt
  (p0:p1:_) <- getIntList
  let modN x = mod x n
  let calc p0 p1 = if
            | p1 == p0 + 1        -> min (modN (n - p0 + 1)) ((p0 - 1) + 2)
            | p0 == n && p1 == 1  -> 1
            | p1 == p0 - 1        -> min (modN p0 + 1) (n - p0 + 1)
            | p0 == 1 && p1 == n  -> 2
            | otherwise -> error "cant sort!"
  print $ calc p0 p1

--------------------------------
-- \/ my template \/
--------------------------------

-- fixed Prelude

{-# INLINE sum #-}
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

{-# INLINE product #-}
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 0

-- IO

{-# INLINE flush #-}
flush :: IO ()
flush = hFlush stdout

{-# INLINE listToTuple #-}
listToTuple :: (Integral a) => [a] -> (a,a)
listToTuple (x: y: _) = (x, y)
listToTuple xs        = error $ "listToTuple error: " ++ show (map fromIntegral xs)

{-# INLINE readInt #-}
readInt :: (Integral a) => BS.ByteString -> a
readInt = fromInteger . fst . fromJust . BS.readInteger
{-# INLINE readIntTuple #-}
readIntTuple :: (Integral a) => BS.ByteString -> (a, a)
readIntTuple = listToTuple . map readInt . BS.words
{-# INLINE readIntList #-}
readIntList :: (Integral a) => BS.ByteString -> [a]
readIntList = map readInt . BS.words

{-# INLINE getBSsN #-}
getBSsN :: (Integral n) => n -> IO [BS.ByteString]
getBSsN n = replicateM (fromIntegral n) BS.getLine
-- getBSsN n = take (fromIntegral n) . BS.lines <$> BS.getContents

{-# INLINE getInt #-}
getInt :: (Integral a) => IO a
getInt = readInt <$> BS.getLine
{-# INLINE getIntsN #-}
getIntsN :: (Integral a, Integral n) => n -> IO [a]
getIntsN n = map readInt <$> getBSsN n
{-# INLINE getIntsAll #-}
getIntsAll :: (Integral a) => IO [a]
getIntsAll = map readInt . BS.lines <$> BS.getContents

{-# INLINE getIntList #-}
getIntList :: (Integral a) => IO [a]
getIntList = readIntList <$> BS.getLine
{-# INLINE getIntListsN #-}
getIntListsN :: (Integral a, Integral n) => n -> IO [[a]]
getIntListsN n = map readIntList <$> getBSsN n
{-# INLINE getIntListsAll #-}
getIntListsAll :: (Integral a) => IO [[a]]
getIntListsAll = map readIntList . BS.lines <$> BS.getContents
{-# INLINE getIntMatrix #-}
getIntMatrix = getIntListsAll

{-# INLINE getIntTuple #-}
getIntTuple :: (Integral a) => IO (a, a)
getIntTuple = readIntTuple <$> BS.getLine
{-# INLINE getIntTuplesN #-}
getIntTuplesN :: (Integral a, Integral n) => n -> IO [(a, a)]
getIntTuplesN n = map readIntTuple <$> getBSsN n
{-# INLINE getIntTuplesAll #-}
getIntTuplesAll :: (Integral a) => IO [(a, a)]
getIntTuplesAll = map readIntTuple . BS.lines <$> BS.getContents

{-# INLINE printYesNo #-}
printYesNo :: Bool -> IO ()
printYesNo bool = print $ if bool then "Yes" else "No"
{-# INLINE printYESNO #-}
printYESNO :: Bool -> IO ()
printYESNO bool = print $ if bool then "YES" else "NO"
