{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Conduit
import Data.Conduit.Combinators qualified as C
import Data.Set qualified as Set

isUniq :: [Char] -> Bool
isUniq cs = sort cs /= sort (Set.toList (Set.fromList cs))

f :: Int -> IO Int
f n = runConduitRes $ sourceFile "input"
  .| C.decodeUtf8
  .| C.concat
  .| C.slidingWindow n
  .| C.takeWhile isUniq
  .| C.length <&> (n +)

main :: IO ()
main = (,) <$> f 4 <*> f 14 >>= print
