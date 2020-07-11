module Player
    ( Role(..)
    , Player(..)
    )
where


import           Deck                           ( Card )
import           Data.Maybe

data Role = Dealer | SmallBlind | BigBlind | OnlyPlayer
    deriving (Show, Eq)

instance Enum Role where
    toEnum n = case n of
        0 -> Dealer
        1 -> SmallBlind
        2 -> BigBlind
        _ -> OnlyPlayer
    fromEnum role = case role of
        Dealer     -> 0
        SmallBlind -> 1
        BigBlind   -> 2
        _          -> 3

data Player = Player
    { hand :: [Card]
    , role :: Role
    , cash :: Int
    }

instance Show Player where
    show (Player _ role cash) = show role ++ ": $" ++ show cash
