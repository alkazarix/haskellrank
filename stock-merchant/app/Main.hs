module Main (main) where

import Data.List

solve :: [Int] -> Int
solve = sum . map (\xs -> div (length xs) 2) . group . sort

main :: IO ()
main = interact $ show . solve . map read . tail . words
