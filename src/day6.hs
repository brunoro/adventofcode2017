import System.Environment
import System.IO()
import Data.Array as A (Array, assocs, listArray, (//), (!))
import Data.Set as S (Set, member, empty, insert)

import Debug.Trace (trace)

inspect :: Show a => a -> a
inspect x = trace (show x) x

readWords :: FilePath -> IO [String]
readWords = fmap words . readFile

parseInt :: String -> Int
parseInt x = read x :: Int

toArray :: [Int] -> Array Int Int
toArray x = listArray (0, length x - 1) x

maxBank :: Array Int Int -> (Int, Int)
maxBank arr =
    let as = assocs arr
    in foldl (\acc x -> if snd x > snd acc then x else acc) (head as) as

next :: Array Int Int -> Int -> Int
next arr i = mod (i + 1) (length arr)

distribute :: Array Int Int -> Int -> Int -> Array Int Int
distribute arr _ 0 = arr
distribute arr i n = distribute (arr // [(i, arr ! i + 1)]) (next arr i) (n - 1)

reallocate :: Array Int Int -> Array Int Int
reallocate arr = 
    let (i, n) = maxBank arr
    in distribute (arr // [(i, 0)]) (next arr i) n

_numReallocs :: Array Int Int -> Set (Array Int Int) -> Int -> Int
_numReallocs arr set i
    | member arr set = i
    | otherwise = _numReallocs (reallocate arr) (insert arr set) (i + 1)

numReallocs :: Array Int Int -> Int
numReallocs arr = _numReallocs arr (S.empty) 0

_loopSize :: Array Int Int -> Array Int Int -> Int -> Int
_loopSize arr orig i
    | arr == orig = i
    | otherwise = _loopSize (reallocate arr) orig (i + 1)

loopSize :: Array Int Int -> Int
loopSize arr = _loopSize (reallocate arr) arr 1

main :: IO ()
main = do
  filename <- fmap head $ getArgs
  banks <- fmap toArray $ fmap (map parseInt) $ readWords $ filename
  print $ numReallocs $ banks
  print $ loopSize $ banks