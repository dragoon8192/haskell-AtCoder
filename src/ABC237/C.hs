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

-- For main.

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
  str :: ByteString <- parseLine
  let
    (asR, csRAsL) = BS.span (=='a') . BS.reverse $ str
    (asL, cs) = BS.span (=='a') . BS.reverse $ csRAsL
    csR = BS.reverse cs
  liftIO $ printYesNo $ BS.length asL <= BS.length asR && cs == csR

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

instance ParseableElement BS.ByteString where
  {-# INLINE parser #-}
  parser = AP.takeTill AP.isSpace

instance ParseableElement String where
  {-# INLINE parser #-}
  parser = BS.unpack <$> AP.takeTill AP.isSpace

instance {-# OVERLAPS #-} (Integral a) => ParseableElement a where
  {-# INLINE parser #-}
  parser = AP.signed AP.decimal

class LineParseable a where
  lineParser :: AP.Parser a
  {-# INLINE parseLine #-}
  parseLine :: Solver a
  parseLine = parse lineParser

instance {-# OVERLAPS #-} (ParseableElement a) => LineParseable a where
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

instance {-# OVERLAPS #-} (LineParseable a, LinesParseableNM m) => LinesParseableN (m a) where
  {-# INLINE parseLinesN' #-}
  parseLinesN' = parseLinesNM'

instance {-# OVERLAPS #-} LinesParseableNM [] where
  {-# INLINE parseLinesNM' #-}
  parseLinesNM' n = AP.count n parseLine

instance {-# OVERLAPS #-} LinesParseableNM V.Vector where
  {-# INLINE parseLinesNM' #-}
  parseLinesNM' n = V.replicateM n parseLine

instance {-# OVERLAPS #-} (LineParseable a, UV.Unbox a) => LinesParseableN (UV.Vector a) where
  {-# INLINE parseLinesN' #-}
  parseLinesN' n = UV.replicateM n parseLine

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
  type BaseInt a :: *
  prime :: BaseInt a
  toIntMod :: BaseInt a -> a
  fromIntMod :: a -> BaseInt a

instance {-# OVERLAPS #-} (IntMod a, Integral (BaseInt a)) => Num a where
  {-# INLINE fromInteger #-}
  fromInteger = toIntMod . fromInteger
  x + y = toIntMod $ fromIntMod x + fromIntMod y
  x * y = fromInteger $ toInteger (fromIntMod x) * toInteger (fromIntMod y)
  abs = id
  {-# INLINE signum #-}
  signum = const 1
  {-# INLINE negate #-}
  negate = toIntMod . negate . fromIntMod

instance {-# OVERLAPS #-} (IntMod a, Enum (BaseInt a)) => Enum a where
  toEnum = toIntMod . toEnum
  fromEnum = fromEnum . fromIntMod

instance {-# OVERLAPS #-} (IntMod a, Real a, Integral (BaseInt a)) => Integral a where
  quotRem x y = (x * invY, 0)
    where
      invY = toIntMod invY' :: a
      (_, invY', _) = exGcd (fromIntMod y) (prime @ a)
  toInteger = toInteger . fromIntMod

newtype IntMod9 = IntMod9 {fromIntMod9 :: Int}
  deriving (Show, Eq, Ord, Real)

instance IntMod IntMod9 where
  type BaseInt IntMod9 = Int
  prime = 998244353
  toIntMod = IntMod9 . flip mod (prime @ IntMod9)
  fromIntMod = fromIntMod9

newtype IntMod10 = IntMod10 {fromIntMod10 :: Int}
  deriving (Show, Eq, Ord, Real)

instance IntMod IntMod10 where
  type BaseInt IntMod10 = Int
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
