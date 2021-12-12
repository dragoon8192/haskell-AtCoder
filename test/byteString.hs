import qualified Data.ByteString.Char8 as BS
main = do
  str <- BS.getLine
readIntegerList :: BS.ByteString -> [Integer]
readIntegerList bStr = case BS.readInteger of
  Nothing -> []
  Just (x,bStr1) -> x: readIntegerList bStr1
