import Data.List
main = print . length . nub . tail . map readInt . lines =<< getContents

readInt = read :: String -> Int
