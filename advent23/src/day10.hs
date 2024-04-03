{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Attoparsec.Text hiding (D)
import Data.List
import qualified Data.Map as M
import qualified Data.HashSet as S
import Data.Maybe
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Pos = (Int, Int)
type Map = M.Map Pos Char
type Path = S.HashSet Pos

parseRow :: Int -> Parser [(Pos, Char)]
parseRow y = zip ((,y) <$> [0..]) <$> many1' anyChar

findStart :: Map -> Pos
findStart m = fst . fromJust . find (('S'==) . snd) $ M.assocs m

findNeighbors :: Map -> Pos -> [Pos]
findNeighbors m (x, y) = 
    let t    = fromJust $ M.lookup (x, y) m          -- current tile at (x,y)
        vds  = fromJust $ lookup t validDirections   -- possible neighbor positions for tile t
        vns  = filter (\dt -> fst dt `elem` vds) validNeighbors  -- mapped to valid neighbor pipes
     in catMaybes $ collect <$> vns
  where
    collect :: (Pos, (Char, Char, Char, Char)) -> Maybe Pos
    collect (dt, (a, b, c, s)) =
        let pos = (x + fst dt, y + snd dt) 
         in M.lookup pos m >>= \nt -> if nt `elem` [a,b,c,s] then Just pos
                                                             else Nothing

    north = (0, -1)
    south = (0, 1)
    east = (1, 0)
    west = (-1, 0)

    validDirections :: [(Char, [Pos])]
    validDirections = [('-', [east, west]),
                       ('|', [north, south]),
                       ('7', [west, south]),
                       ('J', [west, north]),
                       ('L', [east, north]),
                       ('F', [east, south]),
                       -- ironically necessary to find S's replacement
                       ('S', [east, west, north, south])]

    validNeighbors :: [(Pos, (Char, Char, Char, Char))]
    validNeighbors = [(north, ('7', '|', 'F', 'S')),
                      (west,  ('L', '-', 'F', 'S')),
                      (east,  ('J', '-', '7', 'S')),
                      (south, ('J', '|', 'L', 'S'))]

step :: Map -> Pos -> Pos -> Pos
step m from into = head $ filter (from /=) $ findNeighbors m into

-- walk in two directions simultaneously
walk :: Map -> Pos -> Int
walk m start = let (a:b:_) = findNeighbors m start
                in go (start, a) (start, b) 1
  where
    go :: (Pos, Pos) -> (Pos, Pos) -> Int -> Int
    go (a0, a) (b0, b) n = let a' = step m a0 a
                               b' = step m b0 b
                            in if a' == b' then n+1
                                           else go (a, a') (b, b') (n+1)

-- walk the entire path in one direction, collecting the positions
loop :: Map -> Pos -> [Pos]
loop m start = let (a:_) = findNeighbors m start
                in go (start, a) []
  where
    go :: (Pos, Pos) -> [Pos] -> [Pos]
    go (a, b) ps
      | b == start = (a:ps)
      | otherwise  = go (b, step m a b) (a:ps)

xray :: Map -> Path -> Int -> Int -> [String]
xray m path xb yb = fmap traceRow [0..yb-1]
  where
    traceRow :: Int -> String
    traceRow y = go False [0..xb-1]
      where
        go :: Bool -> [Int] -> String
        go _ []          = []
        go inside (x:xs) = let inPath  = S.member (x,y) path
                               toggle  = inPath && verticalTile (x,y)
                               inside' = if toggle then not inside else inside
                               c | inside && not inPath     = 'I'
                                 | not inside && not inPath = 'O'
                                 | otherwise                = fromJust $ M.lookup (x,y) m
                              in c:go inside' xs

        verticalTile :: Pos -> Bool
        verticalTile p = let t = fromJust $ M.lookup p m
                          in t `elem` ("|F7" :: String)

data Loc = L | R | U | D deriving Show

startReplacement :: Map -> Pos -> Char
startReplacement m pos = let (a:b:_) = findNeighbors m pos
                             locA = relativeLocation a
                             locB = relativeLocation b
                          in determine locA locB
  where
    relativeLocation :: Pos -> Loc
    relativeLocation (x, y)
      | x > fst pos = R
      | x < fst pos = L
      | y > snd pos = D
      | otherwise   = U

    determine U D = '|'
    determine D U = '|'
    determine R L = '-'
    determine L R = '-'
    determine R D = 'F'
    determine D R = 'F'
    determine L D = '7'
    determine D L = '7'
    determine L U = 'J'
    determine U L = 'J'
    determine R U = 'L'
    determine U R = 'L'
    determine _ _ = error "RIP determine"


main :: IO ()
main = do
    input <- T.getContents
    let rowsIn =
             fromRight (error "RIP fromRight")
             $ traverse (uncurry (\y -> parseOnly $ parseRow y))
             $ zip [0..] (T.lines input)

    let ybound = length rowsIn
    let xbound = length $ head rowsIn
    let rows = concat rowsIn

    -- part 1
    let m = M.fromList rows
    let start = findStart m
    print $ walk m start
    -- part 2
    let path = S.fromList $ loop m start
    let replacement = startReplacement m start  -- Replace S for the tracing
    let m' = M.update (const $ Just replacement) start m
    print $ sum $ fmap (length . filter (=='I'))
                $ xray m' path xbound ybound
