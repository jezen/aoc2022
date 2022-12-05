{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Conduit
import Data.Char (isDigit)
import Data.Conduit.Combinators qualified as C
import Data.List qualified as L
import Prelude qualified as P
import Text.ParserCombinators.ReadP

type Assignment = ((Integer, Integer), (Integer, Integer))

assignmentParser :: ReadP Assignment
assignmentParser = do
  x1 <- P.read <$> many1 (satisfy isDigit)
  satisfy (== '-')
  y1 <- P.read <$> many1 (satisfy isDigit)
  satisfy (== ',')
  x2 <- P.read <$> many1 (satisfy isDigit)
  satisfy (== '-')
  y2 <- P.read <$> many1 (satisfy isDigit)
  pure ((x1, y1), (x2, y2))

contains :: Assignment -> Bool
contains ((x1, y1), (x2, y2)) = (x1 <= x2 && y1 >= y2) || (x2 <= x1 && y2 >= y1)

overlaps :: Assignment -> Bool
overlaps ((x1, y1), (x2, y2)) = not $ null $ L.intersect [x1..y1] [x2..y2]

go :: (Assignment -> Bool) -> IO Integer
go f = runConduitRes $ sourceFile "input"
  .| C.decodeUtf8
  .| C.linesUnbounded
  .| C.map (fst . P.last . readP_to_S assignmentParser . unpack)
  .| C.filter f
  .| C.length

main :: IO ()
main = do
  go contains >>= print
  go overlaps >>= print
