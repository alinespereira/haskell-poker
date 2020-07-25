module Board
  ( Board (..),
    boardDealer,
    boardSmallBlind,
    boardBigBlind,
    makeBoard,
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

makeBoard :: Int -> Int -> Int -> Board
makeBoard n cash seed = makeNewBoard deck cards (take n [Dealer ..]) players
  where
    (deck, cards) = drawCards (shuffleDeck makeFullDeck seed) 2
    players = []
    makeNewBoard :: [Card] -> [Card] -> [Role] -> [Player] -> Board
    makeNewBoard deck cards [] players =
      Board {common = deck, remaining = cards, players = reverse players}
    makeNewBoard deck cards (r : rs) players =
        let (playerHand, tmpDeck) = drawCards deck 2
          newPlayer = Player {hand = playerHand, role = r, cash = cash}
       in makeNewBoard tmpDeck cards rs (newPlayer : players)
