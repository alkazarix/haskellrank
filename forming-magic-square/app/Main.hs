{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where

import Lib

import Data.List 

type Square = [[Int]]

magic :: Square
magic = [[8, 1, 6],
         [3, 5, 7],
         [4, 9, 2]]

rot90 :: Square -> Square
rot90 = map reverse . transpose

reflect :: Square -> Square
reflect = transpose 

chop :: Int -> [Int] -> Square
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

isMagic :: Square -> Bool
isMagic s = (== 1) $
            length $
            nub $ 
            concat [ map sum s,
                     map sum (transpose s),
                     [sum $ map (uncurry (!!)) $ zip s [1..]],
                     [sum $ map (uncurry (!!)) $ zip (map reverse s) [1..]]]

allMagic' :: [Square]
allMagic'  = filter isMagic $ map (chop 3) $  permutations [1..9]

allMagic :: [Square]
allMagic = concat [
            take 4 $ iterate rot90 magic,
            take 4 $ iterate rot90 $ reflect magic
        ]

cost :: Square -> Square -> Int 
cost s1 s2 =  sum $ map abs $ zipWith (-) (concat s1) (concat s2)

solve :: Square -> Int 
solve s = minimum $ map (cost s) allMagic

main :: IO ()
main = interact  $  show . solve . map (map read . words) . lines
