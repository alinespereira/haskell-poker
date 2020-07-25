module Hand
  ( faceGroups,
    suitGroups,
    countGroupsOfSize,
    maximumGroupSize,
    isSequence,
  )
where

import Data.List
import Deck

faceGroups :: [Card] -> [[Card]]
faceGroups = groupBy (\(Card face _) (Card other _) -> face == other)

suitGroups :: [Card] -> [[Card]]
suitGroups = groupBy (\(Card _ suit) (Card _ other) -> suit == other)

faces :: [Card] -> [Face]
faces = map (\(Card face _) -> face)

suits :: [Card] -> [Suit]
suits = map (\(Card _ suit) -> suit)

countGroupsOfSize :: Int -> [[Card]] -> Int
countGroupsOfSize size groups = length (filter (== size) (map length groups))

maximumGroupSize :: [[Card]] -> Int
maximumGroupSize groups = length (map length groups)

isSequence :: [Card] -> Bool
isSequence (x : xs) =
  sort (faces (x : xs)) == take (length (x : xs)) [cardFace x ..]
isSequence _ = True
