module Main where
import Data.List
import Data.Maybe

dropElement :: Int -> [Int] -> [Int]
dropElement element xs =
  if element `elem` xs then
    takeWhile (/= element) xs ++ tail (dropWhile (/= element) xs)
  else
    xs

dropUntilElement :: Int -> [Int] -> [Int]
dropUntilElement element xs =
  case elemIndex element xs of
    Just n -> drop n xs
    Nothing -> xs

main :: IO ()
main =
  do
    print (dropElement 4 [1..10])
    print (dropElement 11 [1..10])
