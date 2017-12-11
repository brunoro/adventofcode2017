import System.IO
import Data.Map as M (Map, lookup, singleton, insert)
import Debug.Trace (trace)

inspect :: Show a => String -> a -> a
inspect s x = trace (s ++ " " ++ show x) x

dist :: Int -> Int
dist 1 = 0
dist i = let
    s = ceiling $ (sqrt (fromIntegral i) - 1) / 2 -- next square root
    a = (4*s^2 - 2*s + 1) -- distance to axis
    in s + (abs $ mod (i - a) s) -- dist i<>center = axis<>center + dist axis<>i
  where

data Pt = Pt Int Int

up    (x, y) = (x, y + 1)
down  (x, y) = (x, y - 1)
left  (x, y) = (x - 1, y)
right (x, y) = (x + 1, y)

stepTimes :: (a -> a) -> Int -> a -> [a]
stepTimes _ 0 _ = []
stepTimes step n curr = 
    let next = step curr
    in next:(stepTimes step (n - 1) next)

chain :: [(a -> a, Int)] -> a -> [a]
chain [] _ = []
chain ((step, times):xs) start =
    let result = stepTimes step times start
    in result ++ (chain xs (last result))

popLast :: [a] -> ([a], a)
popLast x =
    let rev = reverse x
    in (reverse $ tail $ rev, head rev)

square :: (Int, Int) -> Int -> ([(Int, Int)], (Int, Int))
square start i =
    let (rest, next) = popLast $ chain [(up, 2 * i - 1), (left, 2 * i), (down, 2 * i), (right, 2 * i + 1)] start
    in (start:rest, next)

_walk :: (Int, Int) -> Int -> [(Int, Int)]
_walk start i =
    let (points, next) = square start i
    in points ++ (_walk next (i + 1))

walk :: (Int, Int) -> [(Int, Int)]
walk start = _walk start 1

spiral :: [(Int, Int)]
spiral = walk (1, 0)

value :: Map (Int, Int) Int -> (Int, Int)  -> Int
value m p =
    let val = M.lookup p m
    in maybe 0 id val

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = 
    [(x + i, y + j) | i <- [-1..1], j <- [-1..1], not ((i, j) == (0, 0))]

sumNeighbors :: Map (Int, Int) Int -> (Int, Int) -> Int
sumNeighbors m p = 
    foldl (+) 0 (map (value m) (neighbors p))

testPos :: Map (Int, Int) Int -> (Int, Int) -> (Map (Int, Int) Int, Int)
testPos m p =
    let val = sumNeighbors m p
    in (M.insert p val m, val)

_stressTest :: Map (Int, Int) Int -> [(Int, Int)] -> Int -> (Map (Int, Int) Int, Int)
_stressTest m (p:ps) limit =
    let (nextM, val) = testPos m p
    in if val > limit then (nextM, val) else _stressTest nextM ps limit

second :: (a, b) -> b
second (_, x) = x

stressTest :: Int -> Int
stressTest limit =
    second $ _stressTest (M.singleton (0, 0) 1) spiral limit

main :: IO ()
main = do
    -- print $ dist 277678
    print $ stressTest 277678