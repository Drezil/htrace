{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.ByteString as B
import Data.ByteString.Char8 as B8

import Data.Attoparsec

import Scene.Parser

main :: IO ()
main = do
    f <- B.readFile "scenes/test.sce"
    print $ parseScene f
