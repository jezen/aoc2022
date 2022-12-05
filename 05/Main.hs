{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Conduit
import Data.Char (isDigit)
import Data.Conduit.Combinators qualified as C
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Prelude qualified as P
import Text.ParserCombinators.ReadP

type Procedure = (Int, Int, Int)

type Stack = Map Int String

procedureParser :: ReadP Procedure
procedureParser = do
  string "move "
  q <- P.read <$> many1 (satisfy isDigit)
  string " from "
  f <- P.read <$> many1 (satisfy isDigit)
  string " to "
  t <- P.read <$> many1 (satisfy isDigit)
  pure (q, f, t)

parseData :: Monad m => ConduitT Text Void m (Stack, [Procedure])
parseData = do
  x <- C.takeWhile (/= "")
    .| C.map unpack
    .| C.sinkList
   >>= pure . map reverse . L.transpose
  y <- C.drop 1
    >> C.map (fst . P.last . readP_to_S procedureParser . unpack)
    .| C.sinkList
  let xs = takeWhile (/= ' ') <$> filter (isDigit . P.head) x
  pure (mapFromList ((\(x:xs) -> (P.read [x], xs)) <$> xs), y)

getData :: IO (Stack, [Procedure])
getData = runConduitRes $ sourceFile "input"
  .| C.decodeUtf8
  .| C.linesUnbounded
  .| parseData

f1 :: Stack -> Procedure -> Stack
f1 stack (0, _, _) = stack
f1 stack (q, f, t) = f1 stack' (q - 1, f, t)
  where
  mCrate = P.last <$> lookup f stack
  stack' = case mCrate of
    Nothing -> stack
    Just c  -> Map.adjust (<> [c]) t (Map.adjust P.init f stack)

f2 :: Stack -> Procedure -> Stack
f2 stack (q, f, t) = case mCrates of
  Nothing -> stack
  Just cs ->
    Map.adjust (<> cs) t (Map.adjust (reverse . drop q . reverse) f stack)
  where mCrates = reverse . take q . reverse <$> lookup f stack

main :: IO ()
main = do
  (stacks, procedure) <- getData
  let run f = runConduitPure $ yieldMany procedure .| C.foldl f stacks
  print $ P.last . snd <$> mapToList (run f1)
  print $ P.last . snd <$> mapToList (run f2)
