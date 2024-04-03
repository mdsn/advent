{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics (Generic)
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.HashMap.Strict as M
import Data.Either (fromRight)
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Pos = (Int, Int)
data Dir = N | W | E | S deriving (Eq, Show)

neighborAt :: Dir -> Pos -> Pos
neighborAt N (x,y) = (x,y-1)
neighborAt W (x,y) = (x-1,y)
neighborAt S (x,y) = (x,y+1)
neighborAt E (x,y) = (x+1,y)

data Platform = Platform { width :: Int
                         , height :: Int
                         , tiles :: M.HashMap Pos Tile
                         } deriving (Eq, Show, Generic)
instance Hashable Platform

data Tile = Empty | Round | Cube deriving (Eq, Show, Generic)
instance Hashable Tile

type Seen = M.HashMap Platform Int

printPlatform :: Platform -> IO ()
printPlatform p = forM_ [0..height p-1] printRow
  where
    printRow :: Int -> IO ()
    printRow y = do
        let ts = fmap (\x -> tiles p M.! (x,y)) [0..width p-1]
        let cs = fmap (\t -> case t of Empty -> '.'
                                       Round -> 'O'
                                       Cube  -> '#')
                      ts
        putStrLn cs

parseRow :: Int -> Parser [(Pos, Char)]
parseRow y = zip ((,y) <$> [0..]) <$> many1' anyChar

makeTile :: (Pos, Char) -> (Pos, Tile)
makeTile (p, 'O') = (p, Round)
makeTile (p, '#') = (p, Cube)
makeTile (p, '.') = (p, Empty)
makeTile _ = error "unreachable"

move :: Platform -> Pos -> Pos -> Platform
move p from to = let p' = M.adjust (const Empty) from (tiles p)
                     p'' = M.adjust (const Round) to p'
                  in p { tiles = p'' }

tilt :: Platform -> Dir -> Platform
tilt p dir = go p indices
  where
    indices | dir == N  = [1 .. height p - 1]
            | dir == S  = reverse [0 .. height p - 2]
            | dir == E  = reverse [0 .. width p - 2]
            | otherwise = [1 .. width p - 1]

    go p [] = p
    go p (ix:ixs) = let p' = roll p dir ix
                     in go p' ixs

roll :: Platform -> Dir -> Int -> Platform
roll p dir ix = let rounds = filter (canRoll p dir) $ findRound p dir ix
                 in go p rounds
  where
    go p []     = p
    go p (w:ws) = let p' = move p w (findFurthest p dir w)
                   in go p' ws

findFurthest :: Platform -> Dir -> Pos -> Pos
findFurthest p dir pos =
    case M.lookup pos' (tiles p) of
        Just Empty -> findFurthest p dir pos'
        _          -> pos
  where
    pos' = neighborAt dir pos

canRoll :: Platform -> Dir -> Pos -> Bool
canRoll p dir pos = case M.lookup (neighborAt dir pos) (tiles p) of
                        Just t  -> t == Empty
                        Nothing -> False

findRound :: Platform -> Dir -> Int -> [Pos]
findRound p dir xy = fmap fst
                   $ filter ((==Round) . snd)
                   $ fmap (\w -> (coord w, tiles p M.! coord w)) ixs
  where
    coord w | dir == N || dir == S = (w, xy)
            | otherwise = (xy, w)
    ixs | dir == N || dir == S = [0..width p-1]
        | otherwise = [0..height p-1]

load :: Platform -> Int
load p = sum $ fmap columnLoad [0..width p - 1]
  where
    columnLoad :: Int -> Int
    columnLoad x = let rounds = findRound p N x
                    in sum $ fmap ((height p -) . snd) rounds

runCycles :: Platform -> Int -> Platform
runCycles p n = go p 0
  where
    go p i
      | i == n    = p
      | otherwise = let p' = spin p in go p' (i+1)

detectCycle :: Platform -> (Int, Int)
detectCycle p = go p 1 [0] (M.singleton p 0)
  where
    go :: Platform -> Int -> [Int] -> Seen -> (Int, Int)
    go p i xs seen = let p' = spin p
                      in case M.lookup p' seen of
                            Just j  -> (j, i)
                            Nothing -> go p' (i+1) (i:xs) (M.insert p' i seen)

spin :: Platform -> Platform
spin p = let p'   = tilt p N
             p''  = tilt p' W
             p''' = tilt p'' S
          in tilt p''' E

main :: IO ()
main = do
    input <- T.getContents
    let rowsIn = fromRight (error "RIP parse")
             $ traverse (uncurry (\y -> parseOnly $ parseRow y))
             $ zip [0..] (T.lines input)

    let h = length rowsIn
    let w = length $ head rowsIn
    let rows = concat rowsIn
    let platform = Platform { height = h, width = w, tiles = M.fromList $ fmap makeTile rows }

    let (i, j) = detectCycle platform
    let len = j - i
    let cycles = (1000000000 - i) `div` len
    let closest = len * cycles + i
    let stretch = 1000000000 - closest
    let billionth = i + stretch

    print $ load $ runCycles platform billionth
