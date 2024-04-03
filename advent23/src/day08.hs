{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Attoparsec.Text hiding (take)
import qualified Data.Map as M
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Inst = R | L deriving (Show, Eq)
type Node = (T.Text, T.Text, T.Text)
type NodeMap = M.Map T.Text (T.Text, T.Text)

parseInst :: Parser [Inst]
parseInst = manyTill (char 'R' *> pure R <|> char 'L' *> pure L) endOfLine

parseNode :: Parser Node
parseNode = do
    name <- takeTill (==' ') <* " = ("
    left <- takeTill (==',') <* ", "
    right <- takeTill (==')') <* ")"
    pure $ (name, left, right)

parseInput :: Parser ([Inst], [Node])
parseInput = do
    inst <- parseInst <* endOfLine
    nodes <- parseNode `sepBy1` endOfLine
    pure $ (inst, nodes)

makeMap :: [Node] -> NodeMap
makeMap ns = foldr f M.empty ns
  where
    f (k, l, r) = M.insert k (l, r)

steps :: [Inst] -> NodeMap -> (T.Text -> Bool) -> T.Text -> Int
steps inst m p k0 = go inst k0 0
  where
    go (i:is) k n
      | p k       = n
      | otherwise = go is (step i k) (n+1)
    go [] _ _ = error "RIP steps"

    step :: Inst -> T.Text -> T.Text
    step L k = fst $ m M.! k
    step R k = snd $ m M.! k


walk :: [Inst] -> NodeMap -> Int
walk is m = steps is m (=="ZZZ") "AAA"

ghostwalk :: [Inst] -> [T.Text] -> NodeMap -> [Int]
ghostwalk is starts m = fmap (steps is m ((=='Z') . T.last)) starts

main :: IO ()
main = do
    input <- T.getContents
    let (inst, nodes) = fromRight ([], []) $ parseOnly parseInput input
    let nmap = makeMap nodes
    print $ walk (cycle inst) nmap

    let starts = filter ((=='A') . T.last) $ M.keys nmap
    print $ foldr1 lcm $ ghostwalk (cycle inst) starts nmap
