{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text
import Data.Either (fromRight)
import qualified Data.Text.IO as T

parseInput :: Parser [[Int]]
parseInput = (signed decimal) `sepBy1` char ' ' `sepBy1` endOfLine

run :: [Int] -> Int
run h = go h [reverse h]
  where
    windows2 :: [a] -> [(a, a)]
    windows2 (x:y:[]) = [(x,y)]
    windows2 (x:y:zs) = (x,y) : windows2 (y:zs)

    go :: [Int] -> [[Int]] -> Int
    go xs seqs = let ns = fmap (\(a,b) -> b - a) $ windows2 xs
                     seqs' = ((reverse ns):seqs)
                  in if (all (==0) ns)
                            then unwind seqs'
                            else go ns seqs'

    unwind :: [[Int]] -> Int
    unwind (s0:s1:seqs)
      | null seqs = x
      | otherwise = unwind ((x:s1):seqs)
      where x = head s0 + head s1

main :: IO ()
main = do
    input <- T.getContents
    let histories = fromRight (error "RIP parse") $ parseOnly parseInput input
    print $ sum $ fmap run histories
    print $ sum . fmap run $ fmap reverse histories
