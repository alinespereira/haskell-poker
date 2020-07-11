module Score
    ( Score(..)
    , calculateScore
    )
where

import           Data.List                      ( elemIndex )

import           Deck
import           Hand

data Score =
    HighestCard | OnePair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
    deriving (Eq, Show)

scoreList :: [Score]
scoreList =
    [ HighestCard
    , OnePair
    , TwoPairs
    , ThreeOfAKind
    , Straight
    , Flush
    , FullHouse
    , FourOfAKind
    , StraightFlush
    , RoyalFlush
    ]

instance Ord Score where
    compare score other =
        compare (elemIndex score scoreList) (elemIndex other scoreList)

calculateScore :: [Card] -> (Score, Card)
calculateScore hand | isRoyalFlush hand    = (RoyalFlush, untieCard)
                    | isStraightFlush hand = (StraightFlush, untieCard)
                    | isFourOfAKind hand   = (FourOfAKind, untieCard)
                    | isFullHouse hand     = (FullHouse, untieCard)
                    | isFlush hand         = (Flush, untieCard)
                    | isStraight hand      = (Straight, untieCard)
                    | isThreeOfAKind hand  = (ThreeOfAKind, untieCard)
                    | isTwoPairs hand      = (TwoPairs, untieCard)
                    | isOnePair hand       = (OnePair, untieCard)
                    | otherwise            = (HighestCard, untieCard)
    where untieCard = highestCard hand

highestCard :: [Card] -> Card
highestCard = maximum

isOnePair :: [Card] -> Bool
isOnePair hand =
    let groups = faceGroups hand
    in  countGroupsOfSize 2 groups == 1 && countGroupsOfSize 3 groups == 0

isTwoPairs :: [Card] -> Bool
isTwoPairs hand =
    let groups = faceGroups hand in countGroupsOfSize 2 groups == 2

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind hand =
    let groups = faceGroups hand
    in  maximumGroupSize groups == 3 && countGroupsOfSize 2 groups == 0

isStraight :: [Card] -> Bool
isStraight = isSequence

isFlush :: [Card] -> Bool
isFlush hand = let groups = suitGroups hand in countGroupsOfSize 5 groups == 1

isFullHouse :: [Card] -> Bool
isFullHouse hand =
    let groups = faceGroups hand
    in  countGroupsOfSize 2 groups == 1 && countGroupsOfSize 3 groups == 1

isFourOfAKind :: [Card] -> Bool
isFourOfAKind hand =
    let groups = faceGroups hand in countGroupsOfSize 4 groups == 1

isStraightFlush :: [Card] -> Bool
isStraightFlush hand = isStraight hand && isFlush hand

isRoyalFlush :: [Card] -> Bool
isRoyalFlush hand = isFlush hand && cardFace (minimum hand) == Ten
