import Data.Maybe
import Data.List
import Control.Monad.Fix

main =do
  s <- getLine
  if dream s
  then putStrLn "Yes"
  else putStrLn "No"

gens = ["dream", "dreamer", "erase", "eraser"]
step :: String -> [String]
step ""  = [""]
step str = mapMaybe (`stripPrefix` str) gens

dream :: String -> Bool
dream str = flip fix [str] $
            \loop xs -> case xs of
                "":_  -> True
                []    -> False
                _     -> loop . concatMap step $ xs
