{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NegativeLiterals, OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections, MultiWayIf #-}

-- For template functions.
import Prelude hiding ( sum, product )
import System.IO ( stdin, stdout, hFlush )
import Data.Maybe ( fromJust )
import Data.Either ( fromLeft )
import Data.Bool ( bool )
import Control.Monad ( replicateM )
import Control.Monad.State
    ( MonadTrans(lift), StateT, MonadState(put, get), evalStateT )
import Control.Applicative ( Alternative((<|>)) )
import qualified Data.ByteString.Char8            as BS
import qualified Data.Attoparsec.ByteString.Char8 as AP

-- For main.
import Debug.Trace ()
import Data.Set        (Set)
import Data.Map.Strict (Map)
import Data.IntSet     (IntSet)
import Data.IntMap     (IntMap)
import qualified Data.List          as L
import qualified Data.Set           as S
import qualified Data.Map.Strict    as M
import qualified Data.IntSet        as IS
import qualified Data.IntMap.Strict as IM

--------------------------------
-- /\ my template /\
--------------------------------

main :: IO ()
main = runSolver do
  -- (m, n) <- parseLine $ spaceSepTuple int int
  -- as <- parseLinesN n int
  lift $ putStrLn "Hello, AtCoder!!"

--------------------------------
-- \/ my template \/
--------------------------------

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

{-# INLINE parseLine #-}
parseLine :: AP.Parser a -> Solver a
parseLine parser = parse $ parser <* AP.endOfLine

{-# INLINE parseLinesN #-}
parseLinesN :: (Integral n) => n -> AP.Parser a -> Solver [a]
parseLinesN n parser = parse . AP.count (fromIntegral n) $ parser <* AP.endOfLine

{-# INLINE spaceOrTab #-}
spaceOrTab :: AP.Parser Char
spaceOrTab = AP.char ' ' <|> AP.char '\t'
  -- AP.space は \n も含む

{-# INLINE spaceSepList #-}
spaceSepList :: AP.Parser a -> AP.Parser [a]
spaceSepList = flip AP.sepBy $ AP.many1 spaceOrTab

{-# INLINE spaceSepTuple #-}
spaceSepTuple :: AP.Parser a -> AP.Parser b -> AP.Parser (a, b)
spaceSepTuple pa pb = (,) <$> pa <* AP.many1 spaceOrTab
                          <*> pb

{-# INLINE spaceSepTuple3 #-}
spaceSepTuple3 :: AP.Parser a -> AP.Parser b -> AP.Parser c -> AP.Parser (a, b, c)
spaceSepTuple3 pa pb pc = (,,)  <$> pa <* AP.many1 spaceOrTab
                                <*> pb <* AP.many1 spaceOrTab
                                <*> pc

{-# INLINE int #-}
int :: (Integral a) => AP.Parser a
int = AP.signed AP.decimal

{-# INLINE string #-}
string :: AP.Parser BS.ByteString
string = AP.takeTill AP.isSpace

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

-- fixed Prelude

{-# INLINE sum #-}
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

{-# INLINE product #-}
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 0
