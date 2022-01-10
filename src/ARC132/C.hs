{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf #-}

import Prelude hiding (sum, product)
import Data.List hiding (sum, product)
import System.IO ( stdout, hFlush )
import Control.Monad ( replicateM )
import Data.Maybe ( fromJust )
import qualified Data.ByteString.Char8 as BS
import Debug.Trace

--------------------------------
-- /\ my template /\
--------------------------------

import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import qualified Data.List   as L
import qualified Data.Set    as S
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

main = do
  (n,d) <- getIntTuple
  let
    toMaybe a = if a == -1
      then Nothing
      else Just a
  as <- L.map toMaybe <$> getIntList
  let
    is = [1..n]
    asis = zip as is :: [(Maybe Int, Int)]
    set0 = IS.fromList [n-d..n]
    map0 = M.singleton set0 1
    -- allSet         = IS.fromList [1..n]
    -- undicidedList  = IS.toAscList . IS.difference allSet $ IS.fromList as
    -- emptyList      = map (+1) . elemIndices (-1) $ as
  let
    calc :: (Maybe Int, Int) -> Map IntSet Int -> Map IntSet Int
    calc (ma, i) = traceShowId . M.unionsWith sumMod . M.mapWithKey (innerFunc (ma, i))
      where
        sumMod x y = mod (x + y) 998244353
    innerFunc :: (Maybe Int, Int) -> IntSet -> Int -> Map IntSet Int
    innerFunc (ma, i) is j = M.fromSet (const j) . calcSets (ma, i) $ is
    calcSets :: (Maybe Int, Int) -> IntSet -> Set IntSet
    calcSets (ma, i) = case ma of
      Nothing -> S.map (insertPositiveElem (i - d - 1)) . removes
      Just a  -> S.map (insertPositiveElem (i - d - 1)) . removeA a
    insertPositiveElem :: Int -> IntSet -> IntSet
    insertPositiveElem x = if x > 0
      then IS.insert x
      else id
    removes :: IntSet -> Set IntSet
    removes set = S.map (flip IS.delete set)
                . S.fromAscList . IS.toAscList $ set
     -- IntSet -> Set Int
    removeA :: Int -> IntSet -> Set IntSet
    removeA a set = if IS.member a set
      then S.singleton $ IS.delete a set
      else S.empty
  print . maybe 0 snd . M.lookupMin $ L.foldr calc map0 asis

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
listToTuple xs        = error $ "listToTuple error: " ++ show (L.map fromIntegral xs)

{-# INLINE readInt #-}
readInt :: (Integral a) => BS.ByteString -> a
readInt = fromInteger . fst . fromJust . BS.readInteger
{-# INLINE readIntTuple #-}
readIntTuple :: (Integral a) => BS.ByteString -> (a, a)
readIntTuple = listToTuple . L.map readInt . BS.words
{-# INLINE readIntList #-}
readIntList :: (Integral a) => BS.ByteString -> [a]
readIntList = L.map readInt . BS.words

{-# INLINE getBSsN #-}
getBSsN :: (Integral n) => n -> IO [BS.ByteString]
getBSsN n = replicateM (fromIntegral n) BS.getLine
-- getBSsN n = take (fromIntegral n) . BS.lines <$> BS.getContents

{-# INLINE getInt #-}
getInt :: (Integral a) => IO a
getInt = readInt <$> BS.getLine
{-# INLINE getIntsN #-}
getIntsN :: (Integral a, Integral n) => n -> IO [a]
getIntsN n = L.map readInt <$> getBSsN n
{-# INLINE getIntsAll #-}
getIntsAll :: (Integral a) => IO [a]
getIntsAll = L.map readInt . BS.lines <$> BS.getContents

{-# INLINE getIntList #-}
getIntList :: (Integral a) => IO [a]
getIntList = readIntList <$> BS.getLine
{-# INLINE getIntListsN #-}
getIntListsN :: (Integral a, Integral n) => n -> IO [[a]]
getIntListsN n = L.map readIntList <$> getBSsN n
{-# INLINE getIntListsAll #-}
getIntListsAll :: (Integral a) => IO [[a]]
getIntListsAll = L.map readIntList . BS.lines <$> BS.getContents
{-# INLINE getIntMatrix #-}
getIntMatrix = getIntListsAll

{-# INLINE getIntTuple #-}
getIntTuple :: (Integral a) => IO (a, a)
getIntTuple = readIntTuple <$> BS.getLine
{-# INLINE getIntTuplesN #-}
getIntTuplesN :: (Integral a, Integral n) => n -> IO [(a, a)]
getIntTuplesN n = L.map readIntTuple <$> getBSsN n
{-# INLINE getIntTuplesAll #-}
getIntTuplesAll :: (Integral a) => IO [(a, a)]
getIntTuplesAll = L.map readIntTuple . BS.lines <$> BS.getContents

{-# INLINE printYesNo #-}
printYesNo :: Bool -> IO ()
printYesNo bool = print $ if bool then "Yes" else "No"
{-# INLINE printYESNO #-}
printYESNO :: Bool -> IO ()
printYESNO bool = print $ if bool then "YES" else "NO"
