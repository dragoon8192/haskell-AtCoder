-- import qualified Data.ByteString.Char8 as BS
import Prelude hiding (
  words, lines, length, filter, getLine, getContents, putStr, unlines,
  )
import Data.ByteString.Char8 hiding (
  map,
  )
main = do
  [n, q] <- map readInteger . words <$> getLine :: IO [Integer]
  as <- map read . words <$> getLine :: IO [Integer]
  xs <- map read . lines <$> getContents :: IO [Integer]
  let ans = [length . filter (x <=) $ as | x <- xs]
  putStr . unlines . map show $ ans
