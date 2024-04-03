{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics (Generic)
import Data.Attoparsec.Text
import qualified Data.Map as M
import qualified Data.HashSet as S
import Data.Either (fromRight)
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Dir = N | S | W | E deriving (Show, Eq, Generic)
instance Hashable Dir
data Beam = Beam Pos Dir deriving Show
type Pos = (Int, Int)
data Tile = Empty | Pipe | Backslash | Forward | Dash deriving (Show, Eq)
type Contraption = M.Map Pos Tile
type Energized = S.HashSet (Pos, Dir)

parseRow :: Int -> Parser [(Pos, Char)]
parseRow y = zip ((,y) <$> [0..]) <$> many1' anyChar

mkTile :: Char -> Tile
mkTile '.' = Empty
mkTile '|' = Pipe
mkTile '\\' = Backslash
mkTile '/' = Forward
mkTile '-' = Dash

advance :: Pos -> Dir -> Pos
advance (x,y) N = (x, y-1)
advance (x,y) E = (x+1, y)
advance (x,y) S = (x, y+1)
advance (x,y) W = (x-1, y)

split :: Dir -> (Dir, Dir)
split dir
  | vertical dir = (W, E)
  | otherwise    = (N, S)

redirect :: Tile -> Dir -> Dir
redirect Backslash N = W    -- \
redirect Backslash E = S
redirect Backslash S = E
redirect Backslash W = N
redirect Forward N = E      -- /
redirect Forward E = N
redirect Forward S = W
redirect Forward W = S

energize :: Contraption -> Dir -> Pos -> Energized
energize c dir start = go [Beam start dir] S.empty
  where
    go :: [Beam] -> Energized -> Energized
    go [] visited = visited
    go ((Beam pos dir):bs) visited =
        let pos' = advance pos dir
            visited' = S.insert (pos, dir) visited
            splitBeams = let (d1, d2) = split dir
                             b1 = Beam (advance pos d1) d1
                             b2 = Beam (advance pos d2) d2
                          in (b1:b2:bs)
         in if S.member (pos, dir) visited
                then go bs visited
                else case M.lookup pos c of
                    Nothing -> go bs visited
                    Just Empty -> go ((Beam pos' dir):bs) visited'
                    Just Pipe ->
                        if vertical dir
                            then go ((Beam pos' dir):bs) visited'
                            else go splitBeams visited'
                    Just Dash ->
                        if horizontal dir
                            then go ((Beam pos' dir):bs) visited'
                            else go splitBeams visited'
                    Just Backslash ->
                        let dir' = redirect Backslash dir
                         in go ((Beam (advance pos dir') dir'):bs) visited'
                    Just Forward ->
                        let dir' = redirect Forward dir
                         in go ((Beam (advance pos dir') dir'):bs) visited'

vertical :: Dir -> Bool
vertical N = True
vertical S = True
vertical _ = False

horizontal :: Dir -> Bool
horizontal = not . vertical

energy :: Energized -> Int
energy e = length $ S.map fst e

maximize :: Contraption -> Int -> Int -> Int
maximize c w h = let xs = [0 .. w-1]
                     ys = [0 .. h-1]
                     below = maximum $ fmap (energy . energize c N) $ fmap (,h-1) xs
                     above = maximum $ fmap (energy . energize c S) $ fmap (, 0) xs
                     left  = maximum $ fmap (energy . energize c E) $ fmap (0,) ys
                     right = maximum $ fmap (energy . energize c W) $ fmap (w-1,) ys
                  in maximum [below, above, left, right]

main :: IO ()
main = do
    input <- T.getContents
    let rowsIn = fromRight (error "parse")
             $ traverse (uncurry (\y -> parseOnly $ parseRow y))
             $ zip [0..] (T.lines input)

    let width = length $ head rowsIn
    let height = length rowsIn
    let contraption = M.fromList $ fmap (\(p, c) -> (p, mkTile c)) $ concat rowsIn
    print $ energy $ energize contraption E (0, 0)
    print $ maximize contraption width height

