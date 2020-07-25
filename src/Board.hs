module Board
  ( Board (..),
    boardDealer,
    boardSmallBlind,
    boardBigBlind,
    makeBoard,
    addPlayer,
  )
where

import Deck
import Player

data Board = Board
  { common :: [Card],
    remaining :: [Card],
    players :: [Player]
  }

instance Show Board where
  show = show . players

getPlayerByRole :: Board -> Role -> Player
getPlayerByRole board playerRole =
  head $ filter (\player -> role player == playerRole) (players board)

boardDealer :: Board -> Player
boardDealer = flip getPlayerByRole Dealer

boardSmallBlind :: Board -> Player
boardSmallBlind = flip getPlayerByRole SmallBlind

boardBigBlind :: Board -> Player
boardBigBlind = flip getPlayerByRole BigBlind

makePlayer :: Role -> Int -> [Card] -> ([Card], Player)
makePlayer role cash deck =
  let (hand, newDeck) = drawCards deck 2
      player = Player {hand = hand, role = role, cash = cash}
   in (newDeck, player)

addPlayer :: Role -> Int -> Board -> Board
addPlayer role cash board =
  let deck = remaining board
      (playerHand, newDeck) = drawCards deck 2
      newPlayer = Player {hand = playerHand, role = role, cash = cash}
   in Board
        { common = common board,
          remaining = newDeck,
          players = players board ++ [newPlayer]
        }

makeBoard :: Int -> Board
makeBoard seed = Board {common = common, remaining = remaining, players = []}
  where
    (common, remaining) = drawCards (shuffleDeck makeFullDeck seed) 2