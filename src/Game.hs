module Game where

import           Data.List
import           Data.Maybe                     ( fromJust )

import           Deck
import           Score
import           Player
import           Board

data Stage = CompulsoryBets | PreFlop | Flop | Turn | River | Showdown
    deriving (Show, Eq)

stagesList :: [Stage]
stagesList = [CompulsoryBets, PreFlop, Flop, Turn, River, Showdown]

instance Enum Stage where
    toEnum n = stagesList !! (n `mod` length stagesList)
    fromEnum face = fromJust $ elemIndex face stagesList

choose :: [a] -> Int -> [[a]]
choose xs n = filter ((n ==) . length) $ subsequences xs

makeHands :: [Card] -> [Card] -> [[Card]]
makeHands playerHand boardHand = map (playerHand ++) (boardHand `choose` 3)

scoreHands :: [[Card]] -> [(Score, Card)]
scoreHands = map calculateScore

bestHand :: [(Score, Card)] -> (Score, Card)
bestHand = maximumBy (\(score, _) (other, _) -> compare score other)

run :: Board -> Stage -> Int -> Player
run = undefined
