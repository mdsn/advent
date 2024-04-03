{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text
import Data.Either (fromRight)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import qualified Data.Text as T
import qualified Data.Text.IO as T

card :: Parser Int
card = do
    skipWhile (/=':') *> ":" *> skipSpace
    winners <- (decimal :: Parser Int) `sepBy1'` many1 space
    _       <- skipSpace *> "|" *> skipSpace
    numbers <- decimal `sepBy1'` many1 space
    pure (S.size $ S.intersection (S.fromList winners) (S.fromList numbers))

makeVec :: [T.Text] -> V.Vector Int
makeVec = V.unfoldr parseLine
  where
    eitherMaybe (Right r) = Just r
    eitherMaybe (Left _)  = Nothing

    parseLine :: [T.Text] -> Maybe (Int, [T.Text])
    parseLine []     = Nothing
    parseLine (l:ls) = do
        eitherMaybe (parseOnly card l) >>= \x -> return (x, ls)

points :: Int -> Int
points 0 = 0
points n = 2 ^ (n - 1)

copy :: V.Vector Int -> Int
copy cards = let len = V.length cards
                 ids = [0..len-1]
              in go cards ids len
  where
    go :: V.Vector Int -> [Int] -> Int -> Int
    go _ [] t       = t
    go cm (i:ids) t = let matches = cm V.! i
                          copies  = [i+j | j <- [1..matches]]
                       in go cm (copies ++ ids) (t + matches)

main :: IO ()
main = do
    input <- T.getContents
    -- let cards = makeVec (T.lines input)
    let cards = fromRight (error "No parse") . fmap V.fromList . traverse (parseOnly card) $ T.lines input
    print $ V.sum . V.map points $ cards
    print $ copy cards
