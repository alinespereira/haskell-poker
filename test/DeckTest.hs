module DeckTest where

import           Test.Hspec
import           Deck

makeFullDeckTest :: Spec
makeFullDeckTest = do
    describe "Deck.makeFullDeck" $ do
        it "returns a full deck of 52 cards" $ do
            let deck = makeFullDeck
            length deck `shouldBe` 52

drawCardTest :: Spec
drawCardTest = do
    describe "Deck.drawCard" $ do
        it "returns a card" $ do
            let (Just card, _) = drawCard makeFullDeck
            card `shouldBe` Card Ace Diamonds

shuffleDeckTest :: Spec
shuffleDeckTest = do
    describe "Deck.shuffleDeck" $ do
        it "returns a deck of the same size as input" $ do
            let deck         = makeFullDeck
            let shuffledDeck = shuffleDeck deck 42
            length shuffledDeck `shouldBe` length deck
