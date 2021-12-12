-- import qualified Data.ByteString.Char8 as BS
import Prelude hiding (
  map, words, lines, length, filter, getLine, getContents, putStr, unlines,
  )
import Data.ByteString.Char8
main = do
  [n, q] <- map read . words <$> getLine :: IO [Integer]
  as <- map read . words <$> getLine :: IO [Integer]
  xs <- map read . lines <$> getContents :: IO [Integer]
  let ans = [length . filter (x <=) $ as | x <- xs]
  putStr . unlines . map show $ ans
