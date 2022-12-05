{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import ClassyPrelude
import Conduit
import Data.Conduit.Combinators qualified as C
import Data.Map.Strict qualified as Map

getCalorieMap :: IO [Int]
getCalorieMap = do
  xs <- runConduitRes $ sourceFile "input"
    .| C.decodeUtf8
    .| C.linesUnbounded
    .| C.foldl f (0, mapFromList [])
  pure $ snd <$> Map.toList (snd xs)
  where
  f :: (Int, Map Int Int) -> Text -> (Int, Map Int Int)
  f (c, m) v = case readMay v of
    Just n  -> (c, insertWith (+) c n m)
    Nothing -> (c + 1, m)

main :: IO ()
main = do
  print . sum . take 1 . reverse . sort =<< getCalorieMap
  print . sum . take 3 . reverse . sort =<< getCalorieMap
