import System.Environment
import System.IO()
import Data.Array()
import Data.List
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Set as S

import Debug.Trace (trace)

inspect :: Show a => String -> a -> a
inspect s x = trace (s ++ " " ++ show x) x

readLines :: FilePath -> IO [T.Text]
readLines = fmap T.lines . fmap T.pack . readFile

parseLine :: T.Text -> [T.Text]
parseLine = T.words . T.strip

hasDups :: S.Set T.Text -> [T.Text] -> Bool
hasDups _ [] = False
hasDups s (x:xs) = 
    if S.member x s then True else hasDups (S.insert x s) xs

hasDuplicates :: [T.Text] -> Bool
hasDuplicates t = hasDups S.empty t

hasAnagrams :: [T.Text] -> Bool
hasAnagrams t = hasDuplicates $ map (T.pack . sort . T.unpack) t

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

main :: IO ()
main = do
  filename <- fmap head $ getArgs
  ls <- readLines $ filename
  print $ foldl (+) 0 $ map (boolToInt . not . hasDuplicates . parseLine) $ ls
  print $ foldl (+) 0 $ map (boolToInt . not . hasAnagrams . parseLine) $ ls