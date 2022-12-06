{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Conduit
import Data.Conduit.Combinators qualified as C
import Data.Set qualified as Set

getChars :: IO String
getChars = runConduitRes $ sourceFile "input"
  .| C.decodeUtf8
  .| C.foldlE (\cs c -> cs <> [c]) []

isUniq :: [Char] -> Bool
isUniq cs = sort cs /= sort (Set.toList (Set.fromList cs))

f :: Int -> [Char] -> IO Int
f n xs = runConduit $ yieldMany xs
  .| C.slidingWindow n
  .| C.takeWhile isUniq
  .| C.length <&> (n +)

main :: IO ()
main = do
  chars <- getChars
  f 4 chars  >>= print
  f 14 chars >>= print
