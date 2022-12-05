{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import Conduit
import Data.Conduit.Combinators qualified as C
import Data.Conduit.List qualified as CL
import Data.List qualified as L
import Data.Text qualified as T

f1' :: Text -> Char
f1' t = T.head $ pack $ uncurry L.intersect xs
  where xs = bimap unpack unpack $ toCompartments t

f2' :: [Text] -> Char
f2' xs = T.head (pack (L.foldl1 L.intersect (unpack <$> xs)))

priority :: Map Char Integer
priority = mapFromList $ zip (['a'..'z'] <> ['A'..'Z']) [1..]

getPriority :: Char -> Integer
getPriority = fromMaybe 0 . flip lookup priority

toCompartments :: Text -> (Text, Text)
toCompartments t = T.splitAt (length t `div` 2) t

f1 :: IO Integer
f1 = runConduitRes $ sourceFile "input"
  .| C.decodeUtf8
  .| C.linesUnbounded
  .| C.map (getPriority . f1')
  .| C.sum

f2 :: IO Integer
f2 = runConduitRes $ sourceFile "input"
  .| C.decodeUtf8
  .| C.linesUnbounded
  .| CL.chunksOf 3
  .| C.map (getPriority . f2')
  .| C.sum

main :: IO ()
main = do
  f1 >>= print
  f2 >>= print
