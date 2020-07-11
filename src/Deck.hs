module Deck
    ( Face(..)
    , Suit(..)
    , Card(..)
    , cardFace
    , cardSuit
    , makeDeck
    , makeFullDeck
    , shuffleDeck
    , sampleCard
    , sampleCards
    , drawCard
    , drawCards
    )
where

import           System.Random                  ( mkStdGen )
import           System.Random.Shuffle          ( shuffle' )
import           Data.Maybe                     ( fromJust )
import           Data.Tuple                     ( swap )
import           Data.List                      ( elemIndex )

data Face =
    Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving (Show, Eq)

instance Ord Face where
    compare face other | faceValue face < faceValue other  = LT
                       | faceValue face == faceValue other = EQ
                       | otherwise                         = GT

faceList :: [Face]
faceList =
    [ Ace
    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Jack
    , Queen
    , King
    ]

instance Enum Face where
    toEnum n = faceList !! (n `mod` length faceList)
    fromEnum face = fromJust $ elemIndex face faceList

data Suit =
    Diamonds | Clubs | Hearts | Spades
    deriving (Show, Eq)

suitList :: [Suit]
suitList = [Diamonds, Clubs, Hearts, Spades]

instance Enum Suit where
    toEnum n = suitList !! (n `mod` length suitList)
    fromEnum face = fromJust $ elemIndex face suitList

data Card = Card Face Suit
    deriving (Show, Eq)

instance Ord Card where
    compare (Card face _) (Card other _) = compare face other

cardFace :: Card -> Face
cardFace (Card face _) = face

cardSuit :: Card -> Suit
cardSuit (Card _ suit) = suit

faceValue :: Face -> Int
faceValue face = case face of
    Ace -> length faceList
    _   -> fromJust $ elemIndex face faceList

makeDeck :: [Face] -> [Suit] -> [Card]
makeDeck faces suits = [ Card face suit | face <- faces, suit <- suits ]

makeFullDeck :: [Card]
makeFullDeck =
    let faces = [Ace .. King]
        suits = [Diamonds .. Spades]
    in  makeDeck faces suits

shuffleDeck :: [Card] -> Int -> [Card]
shuffleDeck deck seed = shuffle' deck (length deck) (mkStdGen seed)

sampleCard :: [Card] -> Int -> (Maybe Card, [Card])
sampleCard []     _    = (Nothing, [])
sampleCard [card] _    = (Just card, [])
sampleCard deck   seed = (Just sampledCard, remainingCards)
  where
    sampledCard    = head $ shuffleDeck deck seed
    remainingCards = [ card | card <- deck, card /= sampledCard ]

sampleCards :: [Card] -> Int -> Int -> ([Card], [Card])
sampleCards deck n seed = sampleAnotherCard deck sampled
  where
    sampled = []
    sampleAnotherCard deck sampled
        | length sampled == n = (sampled, deck)
        | null deck = (sampled, deck)
        | otherwise = sampleAnotherCard remainingCards (newCard : sampled)
        where (Just newCard, remainingCards) = sampleCard deck seed

drawCard :: [Card] -> (Maybe Card, [Card])
drawCard []             = (Nothing, [])
drawCard [card        ] = (Just card, [])
drawCard (card : cards) = (Just card, cards)

drawCards :: [Card] -> Int -> ([Card], [Card])
drawCards deck n = splitAt n deck
