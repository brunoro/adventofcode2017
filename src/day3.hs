import System.IO
import Debug.Trace (trace)

inspect :: String -> Int -> Int
inspect s x = trace (s ++ " " ++ show x) x

dist :: Int -> Int
dist 1 = 0
dist i = let
    s = ceiling $ (sqrt (fromIntegral i) - 1) / 2 -- next square root
    a = (4*s^2 - 2*s + 1) -- distance to axis
    in s + (abs $ mod (i - a) s) -- dist i<>center = axis<>center + dist axis<>i
  where

up    (x, y) = (x, y + 1)
down  (x, y) = (x, y - 1)
left  (x, y) = (x - 1, y)
right (x, y) = (x + 1, y)


main :: IO ()
main = do
    print $ dist 277678