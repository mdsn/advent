module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

prefixes :: String -> [String]
prefixes [] = []
prefixes xs = xs : prefixes (tail xs)

match :: String -> String
match line = reverse $ go (prefixes line) []
  where
    go [] ys = ys
    go (x:xs) ys
      | "one" `isPrefixOf` x = go xs ('1':ys)
      | "two" `isPrefixOf` x = go xs ('2':ys)
      | "three" `isPrefixOf` x = go xs ('3':ys)
      | "four" `isPrefixOf` x = go xs ('4':ys)
      | "five" `isPrefixOf` x = go xs ('5':ys)
      | "six" `isPrefixOf` x = go xs ('6':ys)
      | "seven" `isPrefixOf` x = go xs ('7':ys)
      | "eight" `isPrefixOf` x = go xs ('8':ys)
      | "nine" `isPrefixOf` x = go xs ('9':ys)
      | isDigit (head x) = go xs (head x : ys)
      | otherwise = go xs ys

digits :: String -> String
digits s = [head s, last s]

main :: IO ()
main = do
    input <- getContents
    let parsed = fmap ((read :: String -> Int) . digits . match) $ lines input
    print $ sum parsed
