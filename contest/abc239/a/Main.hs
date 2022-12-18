{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NegativeLiterals, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances  #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections, MultiWayIf #-}

-- For template functions.

import Prelude hiding ( sum, product )
import System.IO ( stdin, stdout, hFlush )
import Data.Maybe ( fromJust )
import Data.Either ( fromLeft )
import Data.Bool ( bool )
import Control.Monad ( replicateM )
import Control.Monad.State ( MonadTrans(lift), StateT, MonadState(put, get), evalStateT, liftIO )
import Control.Applicative ( Alternative((<|>)) )
import Control.Arrow ( first, second )
import Data.Tuple ( swap )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8            as BS
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Vector as V
import Data.Vector ( (!), (//) )
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed ( (!), (//) )
import qualified Data.Array.Repa                  as R
import           Data.Array.Repa ( DIM0, DIM1, DIM2, (:.) ((:.)) )

import Debug.Trace
import Data.Maybe
import Data.Set            ( Set )
import Data.Map.Strict     ( Map )
import Data.IntSet         ( IntSet )
import Data.IntMap         ( IntMap )
import qualified Data.List           as L
import qualified Data.Set            as S
import qualified Data.Map.Strict     as M
import qualified Data.IntSet         as IS
import qualified Data.IntMap.Strict  as IM


-- /\ my template /\

main :: IO ()
main = runSolver do
  -- str :: String <- BS.unpack <$> parseLine
  h :: Int <- parseLine
  -- (m, n) :: (Int, Int) <- parseLine
  -- as :: [Int] <- parseLine
  liftIO $ print $ sqrt . fromIntegral $ h * (h + 12800000)

-- \/ my template \/

-- Solver

type Solver = StateT BS.ByteString IO

runSolver :: Solver a -> IO a
runSolver s = evalStateT s BS.empty

-- Parser

parse :: AP.Parser a -> Solver a
parse parser = do
  result <- AP.parseWith getSome parser =<< get
  case result of
    AP.Done input a -> put input >> return a
    _               -> error . fromLeft "" . AP.eitherResult $ result
  where
    getSome = lift $ BS.hGetSome stdin 65535

class ParseableElement a where
  parser :: AP.Parser a
  {-# INLINE parseElement #-}
  parseElement :: Solver a
  parseElement = parse $ parser <* AP.endOfLine

instance ParseableElement BS.ByteString where
  {-# INLINE parser #-}
  parser = AP.takeTill AP.isSpace

instance {-# OVERLAPPABLE #-} (Integral a) => ParseableElement a where
  {-# INLINE parser #-}
  parser = AP.signed AP.decimal

class LineParseable a where
  lineParser :: AP.Parser a
  {-# INLINE parseLine #-}
  parseLine :: Solver a
  parseLine = parse lineParser

instance {-# OVERLAPPABLE #-} (ParseableElement a) => LineParseable a where
  {-# INLINE lineParser #-}
  lineParser = parser <* AP.endOfLine

instance (ParseableElement a) => LineParseable [a] where
  {-# INLINE lineParser #-}
  lineParser = AP.sepBy parser (AP.many1 spaceOrTab) <* AP.endOfLine

instance (ParseableElement a, ParseableElement b) => LineParseable (a, b) where
  {-# INLINE lineParser #-}
  lineParser = (, ) <$> parser <* AP.many1 spaceOrTab
                    <*> parser <* AP.endOfLine

instance (ParseableElement a, ParseableElement b, ParseableElement c) => LineParseable (a, b, c) where
  {-# INLINE lineParser #-}
  lineParser = (, , ) <$> parser <* AP.many1 spaceOrTab
                      <*> parser <* AP.many1 spaceOrTab
                      <*> parser <* AP.endOfLine

class LineParseableWithN a where
  lineParserWithN' :: Int -> AP.Parser a
  {-# INLINE lineParserWithN #-}
  lineParserWithN :: (Integral n) => n -> AP.Parser a
  lineParserWithN = lineParserWithN' . fromIntegral
  {-# INLINE parseLineWithN #-}
  parseLineWithN :: (Integral n) => n -> Solver a
  parseLineWithN = parse . lineParserWithN

instance (ParseableElement a) => LineParseableWithN [a] where
  {-# INLINE lineParserWithN' #-}
  lineParserWithN' n = AP.count n (parser <* (AP.many1 spaceOrTab <|> pure [])) <* AP.endOfLine

instance (ParseableElement a) => LineParseableWithN (V.Vector a) where
  {-# INLINE lineParserWithN' #-}
  lineParserWithN' n = V.replicateM n (parser <* (AP.many1 spaceOrTab <|> pure [])) <* AP.endOfLine

instance (ParseableElement a, UV.Unbox a) => LineParseableWithN (UV.Vector a) where
  {-# INLINE lineParserWithN' #-}
  lineParserWithN' n = UV.replicateM n (parser <* (AP.many1 spaceOrTab <|> pure [])) <* AP.endOfLine

class LinesParseableN a where
  parseLinesN' :: Int -> Solver a
  {-# INLINE parseLinesN #-}
  parseLinesN :: (Integral n) => n -> Solver a
  parseLinesN = parseLinesN' . fromIntegral

class LinesParseableNM m where
  parseLinesNM' :: (LineParseable a) => Int -> Solver (m a)
  {-# INLINE parseLinesNM #-}
  parseLinesNM :: (LineParseable a, Integral n) => n -> Solver (m a)
  parseLinesNM = parseLinesNM' . fromIntegral

instance {-# OVERLAPPABLE #-} (LineParseable a, LinesParseableNM m) => LinesParseableN (m a) where
  {-# INLINE parseLinesN' #-}
  parseLinesN' = parseLinesNM'

instance LinesParseableNM [] where
  {-# INLINE parseLinesNM' #-}
  parseLinesNM' n = AP.count n parseLine

instance LinesParseableNM V.Vector where
  {-# INLINE parseLinesNM' #-}
  parseLinesNM' n = V.replicateM n parseLine

instance (LineParseable a, UV.Unbox a) => LinesParseableN (UV.Vector a) where
  {-# INLINE parseLinesN' #-}
  parseLinesN' n = UV.replicateM n parseLine

class ParseableArray a where
  arrayParserMN' :: Int -> Int -> AP.Parser a
  {-# INLINE arrayParserMN #-}
  arrayParserMN :: (Integral m, Integral n) => m -> n -> AP.Parser a
  arrayParserMN m n = arrayParserMN' (fromIntegral m) (fromIntegral n)
  parseArrayMN :: (Integral m, Integral n) => m -> n -> Solver a
  parseArrayMN m n = parse $ arrayParserMN m n

instance (ParseableElement a, UV.Unbox a) => ParseableArray (R.Array R.U R.DIM2 a) where
  arrayParserMN' m n = R.fromUnboxed (R.Z :. m :. n) <$> UV.replicateM (m * n) (parser <* AP.space)

{-# INLINE spaceOrTab #-}
spaceOrTab :: AP.Parser Char
spaceOrTab = AP.char ' ' <|> AP.char '\t'
  -- AP.space は \n も含む

-- '#', '.' <-> True, False

{-# INLINE hashDotToBool #-}
hashDotToBool :: Char -> Bool
hashDotToBool = (== '#')

{-# INLINE boolToHashDot #-}
boolToHashDot :: Bool -> Char
boolToHashDot = bool '#' '.'

-- IO

{-# INLINE flush #-}
flush :: IO ()
flush = hFlush stdout

{-# INLINE printYesNo #-}
printYesNo :: Bool -> IO ()
printYesNo bool = putStrLn $ if bool then "Yes" else "No"

{-# INLINE printYESNO #-}
printYESNO :: Bool -> IO ()
printYESNO bool = putStrLn $ if bool then "YES" else "NO"

{-# INLINE printUnwordsVector #-}
printUnwordsVector :: (Show a) => V.Vector a -> IO ()
printUnwordsVector = putStr . unwords . V.toList . V.map show

{-# INLINE printUnwordsUVector #-}
printUnwordsUVector :: (Show a, UV.Unbox a) => UV.Vector a -> IO ()
printUnwordsUVector = putStr . unwords . map show . UV.toList

{-# INLINE printUnlinesVector #-}
printUnlinesVector :: (Show a) => V.Vector a -> IO ()
printUnlinesVector = putStr . unlines . V.toList . V.map show

{-# INLINE printUnlinesUVector #-}
printUnlinesUVector :: (Show a, UV.Unbox a) => UV.Vector a -> IO ()
printUnlinesUVector = putStr . unlines . map show . UV.toList

-- calculation

exGcd :: (Integral a) => a -> a -> (a, a, a)
exGcd a 0 = (a, 1, 0)
exGcd a b = (g, y, x - d * y)
  where
    (d, m) = divMod a b
    (g, x, y) = exGcd b m
  -- 拡張Euclidの互除法
  -- a * x + b * y == gcd a b

class IntMod a where
  prime :: Int
  toIntMod :: Int -> a
  fromIntMod :: a -> Int
  inv :: a -> a
  inv x = toIntMod baseInvX
    where
      (_, baseInvX, _) = exGcd (fromIntMod x) (prime @ a)

instance {-# OVERLAPPABLE #-} (IntMod a) => Num a where
  {-# INLINE fromInteger #-}
  fromInteger = toIntMod . fromInteger
  x + y = toIntMod $ fromIntMod x + fromIntMod y
  x * y = fromInteger $ toInteger (fromIntMod x) * toInteger (fromIntMod y)
  abs = id
  {-# INLINE signum #-}
  signum = const 1
  {-# INLINE negate #-}
  negate = toIntMod . negate . fromIntMod

instance {-# OVERLAPPABLE #-} (IntMod a) => Enum a where
  {-# INLINE toEnum #-}
  toEnum = toIntMod . toEnum
  {-# INLINE fromEnum #-}
  fromEnum = fromEnum . fromIntMod

instance {-# OVERLAPPABLE #-} (IntMod a, Real a) => Integral a where
  quotRem x y = (x * inv y, 0)
  toInteger = toInteger . fromIntMod

instance {-# OVERLAPPABLE #-} (IntMod a) => Show a where
  show = show . fromIntMod

instance {-# OVERLAPPABLE #-} (IntMod a) => Eq a where
  x == y = fromIntMod x == fromIntMod y

instance {-# OVERLAPPABLE #-} (IntMod a) => Ord a where
  compare x y = compare (fromIntMod x) (fromIntMod y)

instance {-# OVERLAPPABLE #-} (IntMod a) => Real a where
  toRational = toRational . fromIntMod

newtype IntMod9 = IntMod9 {fromIntMod9 :: Int}

instance IntMod IntMod9 where
  prime = 998244353
  toIntMod = IntMod9 . flip mod (prime @ IntMod9)
  fromIntMod = fromIntMod9

newtype IntMod10 = IntMod10 {fromIntMod10 :: Int}

instance IntMod IntMod10 where
  prime = 1000000007
  toIntMod = IntMod10 . flip mod (prime @ IntMod10)
  fromIntMod = fromIntMod10

factVectN :: Int -> UV.Vector Int
factVectN n = UV.prescanl (*) 1 $ UV.generate (n + 1) (+ 1)

-- fixed Prelude

{-# INLINE sum #-}
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

{-# INLINE product #-}
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 0
