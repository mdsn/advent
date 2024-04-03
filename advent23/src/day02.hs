{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Either (rights)
import qualified Data.Text as T (lines)
import qualified Data.Text.IO as T

data Color = Red | Green | Blue
data Cube = MkCube { num :: Int, color :: Color }
type Round = [Cube]
data Game = MkGame Int [Round]
type RGB = (Int, Int, Int)

valid :: Game -> Bool
valid (MkGame _ rs) = foldr1 (&&) $ fmap validRound rs
  where
    validRound :: Round -> Bool
    validRound = all (\q -> case color q of
                              Red   -> num q <= 12
                              Green -> num q <= 13
                              Blue  -> num q <= 14)

minCubes :: Game -> [RGB]
minCubes (MkGame _ rs) = fmap (go 0 0 0) rs
  where
    go :: Int -> Int -> Int -> [Cube] -> RGB
    go r g b []                  = (r, g, b)
    go r g b (MkCube n Red:qs)   = go (max r n) g b qs
    go r g b (MkCube n Green:qs) = go r (max g n) b qs
    go r g b (MkCube n Blue:qs)  = go r g (max b n) qs

foldCubes :: [RGB] -> RGB
foldCubes qs =
    foldr1 (\(r, g, b) (ra, ga, ba) -> (max r ra, max g ga, max b ba))
           qs

powerCubes :: RGB -> Int
powerCubes (r, g, b) = r * g * b

cube :: Parser Cube
cube = do
    n <- decimal <* space
    c <- Red <$ "red" <|> Green <$ "green" <|> Blue <$ "blue"
    pure $ MkCube n c

rounds :: Parser [Round]
rounds = cube `sepBy1'` ", " `sepBy1'` "; "

game :: Parser Game
game = do
    gid <- "Game " *> decimal
    rs  <- ": "    *> rounds
    pure $ MkGame gid rs

main :: IO ()
main = do
    input <- T.getContents
    let games = rights . fmap (parseOnly game) . T.lines $ input
    print . sum . fmap (\(MkGame gid _) -> gid) $ filter valid games
    print . sum . fmap (powerCubes . foldCubes . minCubes) $ games
