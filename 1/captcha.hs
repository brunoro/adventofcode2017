import System.IO
import Data.Array

seqPairs :: String -> [(Char, Char)]
seqPairs x = zip x ((tail x) ++ [(head x)])

pairValue :: (Char, Char) -> Int
pairValue (x, y)
  | x == y    = (read [x] :: Int)
  | otherwise = 0

solveCaptcha :: String -> Int
solveCaptcha s = foldl (\x y -> x + pairValue y) 0 (seqPairs s)

dropLast :: String -> String
dropLast s = reverse $ drop 1 $ reverse $ s

main :: IO ()
main = do
  numbers <- readFile "input.txt"
  print $ solveCaptcha $ dropLast $ numbers
