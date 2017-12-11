import System.Environment
import System.IO()
import Data.Array
import Data.List
import qualified Data.Text as T

readLines :: FilePath -> IO [T.Text]
readLines = fmap T.lines . fmap T.pack . readFile

parseInt :: T.Text -> Int
parseInt x = read (T.unpack x) :: Int

toArray :: [Int] -> Array Int Int
toArray x = listArray (0, length x) x

inBounds :: Int -> Array Int Int -> Bool
inBounds i arr = let (lo, hi) = bounds arr in i >= lo && i < hi

jump :: (Int -> Int) -> Int -> Array Int Int -> (Int, Array Int Int)
jump inc i state = 
    let op = state ! i 
    in (i + op, state // [(i, inc op)])

_run :: (Int -> Int) -> Int -> Array Int Int -> Int
_run inc i state
    | inBounds i state = (+1) $ uncurry (_run inc) $ jump inc i state
    | otherwise = 0

run :: (Int -> Int) -> [Int] -> Int
run inc = _run inc 0 . toArray

main :: IO ()
main = do
  filename <- fmap head $ getArgs
  instructions <- fmap (map parseInt) $ readLines $ filename
  print $ run (+1) $ instructions
  print $ run (\x -> if x >= 3 then x - 1 else x + 1) $ instructions