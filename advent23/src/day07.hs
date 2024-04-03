{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text
import Data.List
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Kind = HighCard
          | Pair
          | TwoPair
          | ThreeOak
          | FullHouse
          | FourOak
          | FiveOak
          deriving (Show, Enum, Eq, Ord)

data Hand = Hand { kind :: Kind
                 , cards :: T.Text
                 , bid :: Int
                 } deriving (Eq, Show)

strength :: Char -> Int
strength 'T' = 10
strength 'J' = 11
strength 'Q' = 12
strength 'K' = 13
strength 'A' = 14
strength x   = read [x] -- thx deniz

strength2 :: Char -> Int
strength2 'J' = 1
strength2 x = strength x

compareWith :: (Char -> Int) -> Hand -> Hand -> Ordering
compareWith fn a b
  | kind a /= kind b = kind a `compare` kind b
  | otherwise        = compareCards (cards a) (cards b)
  where
    compareCards :: T.Text -> T.Text -> Ordering
    compareCards ca cb = let sa = fn (T.head ca)
                             sb = fn (T.head cb)
                          in if sa == sb
                                then compareCards (T.tail ca) (T.tail cb)
                                else sa `compare` sb

instance Ord Hand where
    compare a b = compareWith strength a b

makeKind :: T.Text -> Kind
makeKind cs = case counts of
            [1,1,1,1,1] -> HighCard
            [1,1,1,2]   -> Pair
            [1,2,2]     -> TwoPair
            [1,1,3]     -> ThreeOak
            [2,3]       -> FullHouse
            [1,4]       -> FourOak
            [5]         -> FiveOak
            _ -> error "RIP makeKind"
  where
    counts = sort
           . fmap T.length
           . T.group
           . T.pack
           . sort
           . T.unpack
           $ cs

changeRules :: Hand -> Hand
changeRules hand = hand { kind = newKind }
  where
    newKind :: Kind
    newKind = let cs = cards hand
                  notj   = T.filter (/='J') cs
                  sorted = T.pack . reverse . sortBy compareOne . T.unpack $ notj
                  groups = reverse .
                           sortOn fst .
                           fmap (\g -> (T.length g, g)) $
                           T.group sorted
                  highest = if T.null notj
                              then "J"
                              else T.take 1 . snd . head $ groups
                  metamorphosed = T.replace "J" highest cs
               in makeKind metamorphosed

    compareOne :: Char -> Char -> Ordering
    compareOne ca cb = strength ca `compare` strength cb
    
parseHand :: Parser Hand
parseHand = do
    cs <- takeTill (==' ') <* space
    b <- decimal
    pure $ Hand (makeKind cs) cs b

parseInput :: Parser [Hand]
parseInput = parseHand `sepBy1` endOfLine

earnings :: [Hand] -> Int
earnings = sum . fmap payout . zip [1..]
  where
    payout (rank, hand) = rank * bid hand

main :: IO ()
main = do
    input <- T.getContents
    let hands = fromRight [] $ (parseOnly parseInput) input
    print $ earnings . sort $ hands
    print $ earnings . sortBy (compareWith strength2) $ fmap changeRules hands
