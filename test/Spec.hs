import           Test.Hspec
import           DeckTest

main :: IO ()
main = hspec $ do
    makeFullDeckTest
    drawCardTest
    shuffleDeckTest

