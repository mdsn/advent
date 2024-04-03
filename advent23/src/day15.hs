{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Map as M
import Data.Char (isLetter, ord)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.IO as T

parseInput :: Parser [T.Text]
parseInput = takeTill (\c -> c == ',' || isEndOfLine c) `sepBy1` char ','

parseStep :: Parser Step
parseStep = do
    label <- takeWhile1 isLetter
    op <- char '=' <|> char '-'
    n <- optional decimal
    return $ if op == '-' then Dash label
                          else Equal label (fromJust n)

type Label = T.Text
data Lens = Lens Label Int deriving Show
type Boxes = M.Map Int [Lens]
data Step = Dash Label
          | Equal Label Int deriving Show

hash :: T.Text -> Int
hash = T.foldl' f 0
  where f n c = let n' = (n + ord c) * 17 in n' `rem` 256

findLens :: Label -> [Lens] -> Maybe Int
findLens l = findIndex (\(Lens l' _) -> l' == l)

initialize :: Boxes -> [Step] -> Boxes
initialize bs [] = bs
initialize bs (Equal l n : ss) =
    let bs' = M.alter (put $ Lens l n) (hash l) bs
     in initialize bs' ss
  where
    put lens Nothing = pure $ [lens]
    put lens@(Lens l _) (Just b) = pure $
        case findLens l b of Nothing -> b ++ [lens]
                             Just i  -> let (h, t) = splitAt i b
                                         in h ++ [lens] ++ tail t
initialize bs (Dash l : ss) =
    let bs' = M.alter (remove l) (hash l) bs
     in initialize bs' ss
  where
    remove l Nothing  = Nothing
    remove l (Just b) = pure $
        case findLens l b of Nothing -> b
                             Just i  -> let (h, t) = splitAt i b
                                         in h ++ tail t

power :: Boxes -> Int
power bs = sum $ fmap (uncurry f) (M.assocs bs)
  where
    f k b = sum $ fmap (g (k+1)) (zip [1..] b)
    g k (slot, Lens _ n) = k * slot * n

main :: IO ()
main = do
    input <- T.getContents
    let steps = fromRight (error "parse") $ parseOnly parseInput input
    print $ sum $ fmap hash steps

    let steps' = fromRight (error "parse 2")
               $ traverse (parseOnly parseStep) steps
    print $ power $ initialize M.empty steps'
