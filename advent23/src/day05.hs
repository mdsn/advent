{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text
import Data.Either
import Data.List (sortOn)
import qualified Data.Text.IO as T

data Triple = Triple { dst :: Int , src :: Int , len :: Int } deriving Show
type Range = (Int, Int)
data LayerMap = LayerMap { mapSrc :: Range , mapDst :: Range } deriving Show

parseSeeds :: Parser [Int]
parseSeeds = "seeds: " *> decimal `sepBy1` space <* endOfLine

parseMap :: Parser [Triple]
parseMap = do
    skipWhile (not . isEndOfLine) *> endOfLine
    triples <- many1 parseTriple
    pure $ fmap (\(d,s,l) -> Triple d s l) triples
  where
    parseTriple :: Parser (Int, Int, Int)
    parseTriple = do
       d <- (decimal :: Parser Int) <* char ' '
       s <- (decimal :: Parser Int) <* char ' '
       l <- (decimal :: Parser Int) <* endOfLine
       pure (d, s, l)

parseInput :: Parser ([Int], [[Triple]])
parseInput = do
    seeds                 <- parseSeeds <* endOfLine
    seedToSoil            <- parseMap <* endOfLine
    soilToFertilizer      <- parseMap <* endOfLine
    fertilizerToWater     <- parseMap <* endOfLine
    waterToLight          <- parseMap <* endOfLine
    lightToTemperature    <- parseMap <* endOfLine
    temperatureToHumidity <- parseMap <* endOfLine
    humidityToLocation    <- parseMap
    pure (seeds, [seedToSoil, soilToFertilizer, fertilizerToWater,
                  waterToLight, lightToTemperature, temperatureToHumidity,
                  humidityToLocation])

-- apply a layer's transformation to a number
mapValue :: Int -> [Triple] -> Int
mapValue x []     = x
mapValue x ((Triple d s l):ts) = if (s <= x) && (x < s+l)
                                    then d + (x - s)
                                    else mapValue x ts

findLocation :: [[Triple]] -> Int -> Int
findLocation = flip (foldl mapValue)

pairs :: [Int] -> [(Int, Int)]
pairs (a:b:[]) = [(a, b)]
pairs (a:b:cs) = (a, b) : pairs cs
pairs _        = error "RIP pairs"

toRange :: (Int, Int) -> Range
toRange (a, l) = (a, a+l-1)

makeLayerMap :: Triple -> LayerMap
makeLayerMap t = LayerMap (toRange (src t, len t)) (toRange (dst t, len t))

intersect :: Range -> Range -> Range
intersect a b = let ((_, a1), (b0, b1)) = order a b
                 in (b0, min a1 b1)
  where
    order a@(a0, _) b@(b0, _)
      | a0 <= b0  = (a, b)
      | otherwise = (b, a)

contains :: Range -> Range -> Bool
contains a b = fst a <= fst b && snd b <= snd a

-- split into intersection and rest of input range, if any
split :: Range -> Range -> [Range]
split a b
  | a `contains` b = splitContains
  | b `contains` a = [a]
  | lo == hi       = [a]
  | otherwise      = if lo == fst a
                        then [(fst a, hi), (hi + 1, snd a)]
                        else [(fst a, lo), (lo + 1, snd a)]
  where
    (lo, hi) = intersect a b
    splitContains
      | a == b          = [a]
      | fst a == fst b  = [(fst a, snd b), (snd b + 1, snd a)]
      | snd a == snd b  = [(fst a, fst b), (fst b + 1, snd b)]
      | otherwise       = [(fst a, fst b), (fst b + 1, snd a)]

-- map output from previous layer to input ranges of current layer,
-- splitting ranges where necessary
mapLayer :: [Range] -> [Range] -> [Range]
mapLayer ir []          = ir
mapLayer [] _           = []
mapLayer (i:ir) (r:rs)
  | i `intersects` r = let (i':ir') = split i r
                        in i' : mapLayer (ir'++ir) (r:rs)
  | otherwise = mapLayer (i:ir) rs
  where
    intersects :: Range -> Range -> Bool
    intersects (a0, a1) (b0, b1) = not disjoint
      where disjoint = (b0 > a1) && (a0 < b1) || (a0 > b1) && (b0 < a1)


layerInputRanges :: [LayerMap] -> [Range]
layerInputRanges m = sortOn fst . foldr (:) [] $ fmap mapSrc m

transform :: [Range] -> [[Range]] -> [[Triple]] -> [Range]
transform rs [] []         = rs
transform rs (l:ls) (m:ms) = let layerIn = mapLayer rs l
                                 layerOut = fmap (\(a,b) -> (mapValue a m, mapValue b m)) layerIn
                              in transform layerOut ls ms
transform _ _ _ = error "RIP transform"

main :: IO ()
main = do
    input <- T.getContents
    let (seeds, maps) = fromRight ([], []) $ parseOnly parseInput input
    let locations = fmap (findLocation maps) seeds
    print $ minimum locations

    let seedRanges = sortOn fst $ fmap toRange $ pairs seeds
    let inputRanges = fmap layerInputRanges $ fmap (fmap makeLayerMap) maps
    print $ fst . head . sortOn fst $ transform seedRanges inputRanges maps
    
