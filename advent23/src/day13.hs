{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text
import Data.Bits
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.IO as T

parsePattern :: Parser [([Int], [Int])]
parsePattern = do
    rows <- many1 (takeWhile1 (not . isEndOfLine) <* endOfLine) `sepBy1` endOfLine
    let irows = fmap (fmap int) rows
    let icols = fmap (fmap int) $ fmap T.transpose rows
    pure $ zip irows icols
  where
    int :: T.Text -> Int
    int t = sum . fmap binary . zip [0..] . reverse $ T.unpack t

    binary :: (Int, Char) -> Int
    binary (n, '#') = 2^n
    binary (_, '.') = 0
    binary _        = error ("binary: unreachable")

diffIsPowerOf2 a b = let d = abs (a-b) in d .&. (d-1) == 0

-- sliding windows of length 2, enumerated by index, filtered by a predicate p
pairIndexes :: [Int] -> (((Int, Int), (Int, Int)) -> Bool) -> [(Int, Int)]
pairIndexes xs p = fmap snd
                 $ filter p
                 $ zip indexed
                 $ tail indexed
  where indexed = zip [0..] xs

type IndexPairingF = [Int] -> [(Int, Int)]
type ValidReflectionF = (Int, ([Int], [Int])) -> Bool
data Refl = CRefl Int | RRefl Int deriving Show

findRefl :: ([Int], [Int]) -> ValidReflectionF -> IndexPairingF -> Refl
findRefl (rows, cols) isValid paired =
    let rrefl = filter isValid $ reflections paired rows
        crefl = filter isValid $ reflections paired cols
     in if null rrefl
            then CRefl $ fst $ head crefl
            else RRefl $ fst $ head rrefl
  where
    reflections :: IndexPairingF -> [Int] -> [(Int, ([Int], [Int]))]
    reflections paired xs = fmap (\(i, _) -> (i, reflect i xs)) $ paired xs

    reflect :: Int -> [Int] -> ([Int], [Int])
    reflect i xs = let (l, r) = splitAt i xs in (reverse l, r)

findValidReflection :: ([Int], [Int]) -> Refl
findValidReflection rc = findRefl rc perfectReflection pair
  where
    -- larger index of two adjacent rows or columns that are equal
    pair :: IndexPairingF
    pair xs = pairIndexes xs (\(a, b) -> snd a == snd b)

    perfectReflection :: ValidReflectionF
    perfectReflection (_, (a, b)) = foldr (&&) True $ fmap (\(a, b) -> a == b) $ zip a b

findSmudgedReflection :: ([Int], [Int]) -> Refl
findSmudgedReflection rc = findRefl rc smudgedReflection pair
  where
    -- larger index of two adjacent rows or cols that are equal, or different by a power of 2
    pair :: IndexPairingF
    pair xs = pairIndexes xs (\(a,b) -> snd a == snd b || diffIsPowerOf2 (snd a) (snd b))

    -- the reflection has a single discrepancy
    smudgedReflection :: ValidReflectionF
    smudgedReflection (_, (l, r)) = 
        let diffs = filter (\(a,b) -> a /= b) $ zip l r
            powTwoDiffs = filter (\(a,b) -> diffIsPowerOf2 a b) diffs
         in (length diffs == 1) && (length powTwoDiffs == 1)

summarize :: [Refl] -> Int
summarize rs = sum $ fmap f rs
  where
    f (RRefl i) = 100*i
    f (CRefl i) = i

main :: IO ()
main = do
    input <- T.getContents
    let parsed = fromRight (error "parse") $ parseOnly parsePattern input
    let refl = fmap findValidReflection parsed
    print $ summarize refl
    let refl' = fmap findSmudgedReflection parsed
    print $ summarize refl'

    -- ambiguous input on line 466:
    --
    --      ....#..      < one row of reflection
    --      .....#.      <
    --      .##.###  <
    --      #..##.#    another row
    --      #..##.#    of reflection
    --      .##..##  <
    --      .....#.
    --
    -- Manually corrected result:
    -- >>> 32806-100+400
    -- 33106

