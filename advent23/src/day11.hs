{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.List

type Pos = (Int, Int)

parseRow :: Int -> String -> [Pos]
parseRow y row = go row [0..] []
  where
    go :: String -> [Int] -> [Pos] -> [Pos]
    go "" _ gs            = gs
    go ('#':cs) (x:xs) gs = go cs xs ((x,y):gs)
    go (_:cs) (_:xs) gs   = go cs xs gs
    go _ _ _              = error "parseRow: unreachable"

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = fmap (x,) xs ++ pairs xs

intersections :: Int -> Int -> [Int] -> Int
intersections a b ws = length $ filter (\w -> a < w && w < b || b < w && w < a) ws

manhattan :: Pos -> Pos -> Int
manhattan (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

ultraManhattan :: Int -> [Int] -> [Int] -> Pos -> Pos -> Int
ultraManhattan m rs cs a b = let dist = manhattan a b
                                 rowInt = (m-1) * (intersections (fst a) (fst b) cs)
                                 colInt = (m-1) * (intersections (snd a) (snd b) rs)
                              in dist + rowInt + colInt

main :: IO ()
main = do
    input <- getContents
    let linesIn = lines input
    let galaxies = concat
                 $ fmap (uncurry parseRow)
                 $ zip [0..] linesIn
    let ix = fmap fst . filter (all (=='.') . snd) . zip [0..]
    let rows = ix linesIn
    let cols = ix $ transpose linesIn
    let multiplier = 1000000
    print $ sum $ fmap (uncurry $ ultraManhattan multiplier rows cols) $ pairs galaxies
