{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Conduit
import Data.Conduit.Combinators qualified as C

f1 :: Text -> Integer
f1 = \case
  "A X" -> 4
  "A Y" -> 8
  "A Z" -> 3
  "B X" -> 1
  "B Y" -> 5
  "B Z" -> 9
  "C X" -> 7
  "C Y" -> 2
  "C Z" -> 6

f2 :: Text -> Integer
f2 = \case
  "A X" -> 3
  "A Y" -> 4
  "A Z" -> 8
  "B X" -> 1
  "B Y" -> 5
  "B Z" -> 9
  "C X" -> 2
  "C Y" -> 6
  "C Z" -> 7

run :: (Text -> Integer) -> IO Integer
run f = runConduitRes $ sourceFile "input"
  .| C.decodeUtf8
  .| C.linesUnbounded
  .| C.map f
  .| C.sum

main :: IO ()
main = do
  run f1 >>= print
  run f2 >>= print
