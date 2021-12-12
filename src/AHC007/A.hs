import Control.Monad.Reader
    ( replicateM,
      MonadTrans(lift),
      MonadReader(ask),
      ReaderT(runReaderT) )
import System.IO ( stdout, hFlush )
import Data.Maybe ( fromJust )
import qualified Data.ByteString.Char8 as BS

flush = hFlush stdout

listToTuple :: (Show a) => [a] -> (a,a)
listToTuple (x: y: _) = (x, y)
listToTuple xs = error $ "listToTuple error " ++  show xs

readInt = fromIntegral . fst . fromJust . BS.readInteger
readIntTuple = listToTuple . map readInt . BS.words

getNBSs :: (Integral a) => a -> IO [BS.ByteString]
getNBSs n = replicateM (fromIntegral n) BS.getLine

getInt = readInt <$> BS.getLine
getIntTuple = readIntTuple <$> BS.getLine
getIntNTuples n = map readIntTuple <$> getNBSs n

main = do
  points <- getIntNTuples 400 :: IO [Point]
  nodeDoms <- getIntNTuples 1995 :: IO [NodeDom]
  let nodes = zip nodeDoms . repeat $ Nothing :: [Node]
  flip runReaderT points $ loop [] nodes

loop :: [Node] -> [Node] -> CalcState ()
loop decided (n:nodes) = do
  l <- lift getInt
  b <- existsShorterPath l n (decided ++ nodes)
  if b
  then do
    lift $ print 1
    lift flush
    loop (decide l n : decided) nodes
  else do
    lift $ print 0
    lift flush
    loop decided nodes
loop _ [] = return ()

decide :: Int -> Node -> Node
decide l (ij,Nothing) = (ij, Just l)
decide _ _ = undefined

type Point = (Int, Int)
type NodeDom = (Int, Int)
type NodeSt = Maybe Int
type Node = (NodeDom, NodeSt)
type CalcState = ReaderT [Point] IO

expectedLen :: Node -> CalcState Int
expectedLen (_, Just l) = return l
expectedLen ((i, j), Nothing) = (2 *) <$> distance i j

distance :: Int -> Int -> CalcState Int
distance i j = do
  ps <- ask
  let (xi, yi) = ps !! i
  let (xj, yj) = ps !! i
  return . round .  sqrt . fromIntegral $ (xi - xj)^2 + (yi - yj)^2

existsShorterPath :: Int -> Node-> [Node] -> CalcState Bool
existsShorterPath l (ij,_) ns = not . null <$> shorterPaths l ij ns

shorterPaths :: Int -> NodeDom -> [Node] -> CalcState [Bool]
shorterPaths l (i, j) (n@((ni,nj), _):ns) = do
  nl <- expectedLen n
  if ni == i && nl < l
  then  if nj == j
        then return [True]
        else do
          step <- shorterPaths (l - nl) (nj, j) ns
          hold <- shorterPaths l (i, j) ns
          return $ step ++ hold
  else shorterPaths l (i, j) ns
shorterPaths _ _ [] = return []

