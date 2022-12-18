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
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8            as BS
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- For main.

import Debug.Trace
import Data.Tuple
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
  n :: Int <- parseLine
  as :: [Int] <- parseLine
  let
    d = func as
    func (a: b: cs) = if a > b
      then a
      else func (b : cs)
    func [a] = a
    func [] = 0
  lift $ putStrLn . unwords . L.map show . filter (/= d) $ as

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

class ParseableElement a where
  parser :: AP.Parser a

instance ParseableElement BS.ByteString where
  {-# INLINE parser #-}
  parser = AP.takeTill AP.isSpace

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

{-# INLINE spaceOrTab #-}
spaceOrTab :: AP.Parser Char
spaceOrTab = AP.char ' ' <|> AP.char '\t'
  -- AP.space は \n も含む

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
printYesNo bool = print $ if bool then "Yes" else "No"

{-# INLINE printYESNO #-}
printYESNO :: Bool -> IO ()
printYESNO bool = print $ if bool then "YES" else "NO"

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

-- fixed Prelude

{-# INLINE sum #-}
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

{-# INLINE product #-}
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 0
