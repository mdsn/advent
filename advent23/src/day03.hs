{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.Read as T (decimal)
import qualified Data.Text.IO as T

type Pos = (Int, Int)
data Number = MkNumber { num :: Int
                       , len :: Int
                       , numPos :: Pos
                       } deriving Show
data Symbol = MkSymbol { sym :: Char
                       , symPos :: Pos
                       } deriving Show
data Row = MkRow { nums :: [Number]
                 , syms :: [Symbol]
                 } deriving Show
type SymbolMap = M.Map Pos Symbol
type AsteriskMap = M.Map Pos [Int]

int :: T.Text -> Int
int = fst . fromRight (0, "") . T.decimal

parseRow :: T.Text -> Int -> Row
parseRow row y = go row 0 [] []
  where
    go :: T.Text
       -> Int
       -> [Number]
       -> [Symbol]
       -> Row
    go "" _ ns ss = MkRow ns ss
    go cs x ns ss
      | isDigit (T.head cs) = let (nstr, rest) = T.span isDigit cs
                                  nlen         = T.length nstr
                                  n            = MkNumber (int nstr) nlen (x, y)
                               in go rest (x + nlen) (n:ns) ss
      | (T.head cs) == '.'  = go (T.tail cs) (x+1) ns ss
      | otherwise           = let s = MkSymbol (T.head cs) (x, y)
                               in go (T.tail cs) (x+1) ns (s:ss)

foldRows :: [Row] -> Row
foldRows = foldr1 (\r ra -> MkRow (nums r ++ nums ra) (syms r ++ syms ra))

makeSymbolMap :: Row -> SymbolMap
makeSymbolMap = M.fromList . fmap (\s -> (symPos s, s)) . syms

surroundingCoords :: Number -> [Pos]
surroundingCoords n = let (x0, y0) = numPos n
                       in [(x, y0-1) | x <- [x0-1 .. x0+len n]]
                       ++ [(x0-1, y0), (x0+len n, y0)]
                       ++ [(x, y0+1) | x <- [x0-1 .. x0+len n]]

filterAdjacent :: [Number] -> SymbolMap -> [Number]
filterAdjacent ns m = filter adjacentSymbols ns
  where
    adjacentSymbols :: Number -> Bool
    adjacentSymbols n = any (flip M.member m) $ surroundingCoords n

makeAsteriskMap :: SymbolMap -> AsteriskMap
makeAsteriskMap = M.mapMaybe (\s -> if sym s == '*' then Just [] else Nothing)

findGears :: [Number] -> AsteriskMap -> AsteriskMap
findGears [] m     = M.filter (\xs -> length xs == 2) m
findGears (n:ns) m = let updated = foldr (M.adjust (num n:)) m (surroundingCoords n)
                      in findGears ns updated

main :: IO ()
main = do
    input <- T.getContents
    let parsed = foldRows . fmap (uncurry parseRow) $ zip (T.lines input) [0..]
    let symbols = makeSymbolMap parsed
    let adjacent = filterAdjacent (nums parsed) symbols
    print . sum $ fmap num adjacent
    let gears = findGears adjacent $ makeAsteriskMap symbols
    print . sum . M.elems $ M.map product gears
