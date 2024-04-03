{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text
import qualified Data.Map as M
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    input <- T.getContents
    print "OK"
