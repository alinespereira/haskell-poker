import           Test.Hspec
import           DeckTest
import           ScoreTest

main :: IO ()
main = hspec $ do
    makeFullDeckTest
    drawCardTest
    shuffleDeckTest

    highestCardTest
    isOnePairTest
    isTwoPairsTest
    isThreeOfAKindTest
    isStraightTest
    isFlushTest
    isFullHouseTest
    isFourOfAKindTest
    isStraightFlushTest
    isRoyalFlushTest
