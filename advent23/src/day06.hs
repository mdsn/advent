{-# LANGUAGE OverloadedStrings #-}
module Main where

ways :: (Int, Int) -> [Int]
ways (t, d) = filter (> d) $ fmap travel [1..t-1]
  where
    travel t' = (t - t') * t'

main :: IO ()
main = do
    -- print . product $ fmap (length . ways) [(41, 244), (66, 1047), (72, 1228), (66, 1040)]
    -- print . length $ ways (41667266, 244104712281040)
    print $ fmap (length . ways) [(41, 244), (66, 1047), (72, 1228), (66, 1040)]
