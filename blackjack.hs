module Main where


data Suit = Spade | Heart | Diamond | Club
data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King

data Card = Card
    { suit :: Suit
    , value :: Value
    }

type Hand = [Card]

hasValueTen :: Value -> Bool
hasValueTen Ten = True
hasValueTen Jack = True
hasValueTen Queen = True
hasValueTen King = True
hasValueTen _ = False

hasBlackjack :: Hand -> Bool
hasBlackjack ((Card _ Ace):(Card _ y):[]) = hasValueTen y
hasBlackjack ((Card _ x):(Card _ Ace):[]) = hasValueTen x
hasBlackjack _ = False
