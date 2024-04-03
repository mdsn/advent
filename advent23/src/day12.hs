{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Attoparsec.Text hiding (take)
import Data.Maybe
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as T

parseLine :: Parser (T.Text, [Int])
parseLine = (,) <$> takeTill isHorizontalSpace <* space
                <*> decimal `sepBy1` char ','

working :: T.Text -> Int
working = T.count "#"

questions :: T.Text -> Int
questions = T.count "?"

replace :: T.Text -> Char -> T.Text
replace t c
  | T.null b   = t
  | otherwise  = T.append a $ T.cons c $ T.tail b
  where (a, b) = T.breakOn "?" t

reject :: T.Text -> [Int] -> Bool
reject t ws = go t ws
  where
    go :: T.Text -> [Int] -> Bool
    go ts []     = isJust $ T.find (=='#') ts
    go ts (x:xs)
      | T.null ts || T.length ts < x = True
      | T.head ts == '.' = let rest = T.dropWhile (=='.') ts
                            in go rest (x:xs)
      | otherwise = let (ts', rest) = T.splitAt (x+1) ts
                        (pref, sep) = T.splitAt x ts'
                     in T.any (=='.') pref || sep == "#" || go rest xs

sections :: T.Text -> [T.Text]
sections t = filter (not . T.null) $ T.splitOn "." t

isConstantGroup :: T.Text -> Bool
isConstantGroup g = T.all (=='#') g

-- simplify constant groups at the beginning and end of gs/xs
-- simplify :: T.Text -> [Int] -> ([T.Text], [Int])
-- simplify t xs = simplifyLeft (sections t) xs [] []
--   where
--     simplifyLeft :: [T.Text] -> [Int] -> [T.Text] -> [Int] -> ([T.Text], [Int])
--     simplifyLeft [] [] gs' xs' = (reverse gs', reverse xs')
--     simplifyLeft [] xs gs' xs' = (reverse gs', reverse xs' ++ xs)
--     simplifyLeft gs [] gs' xs' = (reverse gs' ++ gs, xs')
--     simplifyLeft (g:gs) (x:xs) gs' xs' = if isConstantGroup g && working g == x
--                                             then simplifyLeft gs xs gs' xs'
--                                             else simplifyLeft gs xs (g:gs') (x:xs')

workingGroups :: [T.Text] -> [Int]
workingGroups gs = fmap working gs

groupsMatch :: [T.Text] -> [Int] -> Bool
groupsMatch [] [] = True
groupsMatch [] _  = False
groupsMatch _  [] = False
groupsMatch (g:gs) (x:xs)
  | isConstantGroup g && working g /= x = False
  | not (isConstantGroup g) && T.length (constantPrefix g) > x = False
  | otherwise = groupsMatch gs xs

constantPrefix t = fst $ T.span (=='#') t

bt :: T.Text -> [Int] -> Int
bt t xs
  -- more working springs than expected, reject
  | wt > sx = 0
  -- not possible to have enough working springs, reject
  | wt + qt < sx = 0
  | qt == 0 = if reject t xs then 0 else 1
  | wt == sx = if reject (T.replace "?" "." t) xs then 0 else 1 
  | otherwise = bt (replace t '#') xs + bt (replace t '.') xs
  where
    wt = working t
    qt = questions t
    sx = sum xs
    gs = sections t
    -- (gs', xs') = simplify t xs
    -- t' = T.intercalate "." gs'

generate :: T.Text -> [T.Text]
generate t
  | questions t > 0 = generate (replace t '#') ++ generate (replace t '.')
  | otherwise       = [t]

supersize :: T.Text -> [Int] -> (T.Text, [Int])
supersize t xs = (T.intercalate "?" $ five t, concat $ five xs)
  where five w = take 5 (repeat w)

main :: IO ()
main = do
    input <- T.getContents
    let rows = fromRight (error "no parse") $ traverse (parseOnly parseLine) $ T.lines input
    -- print $ fmap (sections . fst) rows

    -- print $ sum $ fmap (uncurry bt) rows
    -- let r = ("????.######..#####.", [1,6,5])
    -- let r = (".###.???????",[3,2,1])
    -- print $ simplify (fst r) (snd r)
    -- print $ uncurry bt $ r

    let bigboy = [("???.###????.###????.###????.###????.###",[1,1,3,1,1,3,1,1,3,1,1,3,1,1,3]),
                  (".??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##.?.??..??...?##.",[1,1,3,1,1,3,1,1,3,1,1,3,1,1,3]),
                  ("?#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#?",[1,3,1,6,1,3,1,6,1,3,1,6,1,3,1,6,1,3,1,6]),
                  ("????.#...#...?????.#...#...?????.#...#...?????.#...#...?????.#...#...",[4,1,1,4,1,1,4,1,1,4,1,1,4,1,1]),
                  ("????.######..#####.?????.######..#####.?????.######..#####.?????.######..#####.?????.######..#####.",[1,6,5,1,6,5,1,6,5,1,6,5,1,6,5]),
                  ("?###??????????###??????????###??????????###??????????###????????",[3,2,1,3,2,1,3,2,1,3,2,1,3,2,1])]

    -- let bigboy = fmap (uncurry supersize) rows
    print $ uncurry bt $ bigboy !! 1

    -- print $ sum $ fmap (uncurry bt) bigboy
