module ScoreTest where

import           Test.Hspec
import           Deck
import           Score

highestCardTest :: Spec
highestCardTest =
    describe "Score.highestCard" $ it "returns the hand highest card" $ do
        let hand =
                [ Card Ace   Clubs
                , Card Four  Hearts
                , Card Seven Diamonds
                , Card King  Clubs
                , Card Two   Spades
                ]
        calculateScore hand `shouldBe` (HighestCard, Card Ace Clubs)

isOnePairTest :: Spec
isOnePairTest =
    describe "Score.isOnePair" $ it "checks if hand is a OnePair" $ do
        let hand =
                [ Card King  Clubs
                , Card King  Hearts
                , Card Seven Diamonds
                , Card Two   Clubs
                , Card Five  Spades
                ]
        let (score, _) = calculateScore hand
        score `shouldBe` OnePair

isTwoPairsTest :: Spec
isTwoPairsTest =
    describe "Score.isTwoPairs" $ it "checks if hand is a TwoPairs" $ do
        let hand =
                [ Card King  Clubs
                , Card King  Hearts
                , Card Seven Diamonds
                , Card Seven Clubs
                , Card Five  Spades
                ]
        let (score, _) = calculateScore hand
        score `shouldBe` TwoPairs


isThreeOfAKindTest :: Spec
isThreeOfAKindTest =
    describe "Score.isThreeOfAKind" $ it "checks if hand is a ThreeOfAKind" $ do
        let hand =
                [ Card King  Clubs
                , Card King  Hearts
                , Card King  Diamonds
                , Card Seven Clubs
                , Card Five  Spades
                ]
        let (score, _) = calculateScore hand
        score `shouldBe` ThreeOfAKind


isStraightTest :: Spec
isStraightTest =
    describe "Score.isStraight" $ it "checks if hand is a Straight" $ do
        let hand =
                [ Card Three Clubs
                , Card Four  Hearts
                , Card Five  Diamonds
                , Card Six   Clubs
                , Card Seven Spades
                ]
        let (score, _) = calculateScore hand
        score `shouldBe` Straight


isFlushTest :: Spec
isFlushTest = describe "Score.isFlush" $ it "checks if hand is a Flush" $ do
    let hand =
            [ Card King  Clubs
            , Card Queen Clubs
            , Card Nine  Clubs
            , Card Eight Clubs
            , Card Two   Clubs
            ]
    let (score, _) = calculateScore hand
    score `shouldBe` Flush


isFullHouseTest :: Spec
isFullHouseTest =
    describe "Score.isFullHouse" $ it "checks if hand is a FullHouse" $ do
        let hand =
                [ Card King  Clubs
                , Card King  Hearts
                , Card King  Diamonds
                , Card Seven Clubs
                , Card Seven Spades
                ]
        let (score, _) = calculateScore hand
        score `shouldBe` FullHouse


isFourOfAKindTest :: Spec
isFourOfAKindTest =
    describe "Score.isFourOfAKind" $ it "checks if hand is a FourOfAKind" $ do
        let hand =
                [ Card King Clubs
                , Card King Hearts
                , Card King Diamonds
                , Card King Spades
                , Card Two  Hearts
                ]
        let (score, _) = calculateScore hand
        score `shouldBe` FourOfAKind


isStraightFlushTest :: Spec
isStraightFlushTest =
    describe "Score.isStraightFlush"
        $ it "checks if hand is a StraightFlush"
        $ do
              let hand =
                      [ Card Three Clubs
                      , Card Four  Clubs
                      , Card Five  Clubs
                      , Card Six   Clubs
                      , Card Seven Clubs
                      ]
              let (score, _) = calculateScore hand
              score `shouldBe` StraightFlush


isRoyalFlushTest :: Spec
isRoyalFlushTest =
    describe "Score.isRoyalFlush" $ it "checks if hand is a RoyalFlush" $ do
        let hand =
                [ Card Ten   Hearts
                , Card Jack  Hearts
                , Card Queen Hearts
                , Card King  Hearts
                , Card Ace   Hearts
                ]
        let (score, _) = calculateScore hand
        score `shouldBe` RoyalFlush


