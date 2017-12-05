import System.Environment
import System.IO
import Data.Array
import qualified Data.Text as T

readLines :: FilePath -> IO [T.Text]
readLines = fmap T.lines . fmap T.pack . readFile

parseInt :: T.Text -> Int
parseInt x = read (T.unpack x) :: Int

parseLine :: T.Text -> [Int]
parseLine = map parseInt . T.words

_minMax :: (Int, Int) -> Int -> (Int, Int)
_minMax (low, high) x = (min low x, max high x)

-- the clean way to implement this would be using Maybe Int, 
-- but I know for sure that my input won't have big numbers
minMax :: [Int] -> (Int, Int)
minMax = foldl _minMax (99999, 0)

sub :: (Int, Int) -> Int
sub (x, y) = y - x

main :: IO ()
main = do
  filename <- fmap head $ getArgs
  lines <- readLines $ filename
  print $ foldl (+) 0 $ map (sub . minMax . parseLine) $ lines
