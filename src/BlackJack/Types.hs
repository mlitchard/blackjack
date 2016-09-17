{-# LANGUAGE OverloadedStrings #-}
module BlackJack.Types 
  ( CardRank (..)
  , printCardRank
  , printSuit
  , Card (..)
  , printCard 
  , Hand
  , Placed
  , Dealt
  , Bet (..)
  , BankRoll (..)
  , Value (..)
  , PInt 
  , toPInt
  , fromPInt
  , ) where

import BasicPrelude

import Data.Text (Text)
import qualified Data.List.NonEmpty as NonEmpty

data CardRank = Ace | Jack | Queen | King | Pip Int

printCardRank :: CardRank -> Text
printCardRank Ace     = "Ace"
printCardRank Jack    = "Jack"
printCardRank Queen   = "Queen"
printCardRank King    = "King"
printCardRank (Pip 2) = "2"
printCardRank (Pip 3) = "3"
printCardRank (Pip 4) = "4"
printCardRank (Pip 5) = "5"
printCardRank (Pip 6) = "6"
printCardRank (Pip 7) = "7"
printCardRank (Pip 8) = "8"
printCardRank (Pip 9) = "9"
printCardRank (Pip 10) = "10"
printCardRank _ = error ("Not a valid CardRank")

data Suit = Hearts | Clubs | Spades | Diamonds

printSuit :: Suit -> Text
printSuit Hearts   = "Hearts"
printSuit Clubs    = "Clubs"
printSuit Spades   = "Spades"
printSuit Diamonds = "Diamonds"

data Card = Card CardRank Suit

printCard :: Card -> Text
printCard (Card c_rank suit) =
  (printCardRank c_rank) <> " of " <> (printSuit suit)

type Hand = NonEmpty.NonEmpty Card
data Placed = FaceUp | FaceDown deriving Show

type Dealt = [(Card,Placed)]

newtype Bet = Bet {fromBet :: Int}
newtype BankRoll = BackRoll {fromBankRoll :: Int}
newtype Value = Value {fromValue :: Int}

data Winner a = Player a | House

printWinner :: (Show a) => Winner a -> Text
printWinner (Player player) = (pack player) <> " won this round"
printWinner House           = 
  "The house won this round. The house always wins in the end."

printHand :: Hand -> Text
printHand hand = (concat . NonEmpty.map printCard) hand

newtype PInt = PInt {fromPInt :: Int} deriving (Read)

instance Num PInt where
  x - y = x `truncSub` y
            where
              truncSub (PInt x') (PInt y')
                | y' > x'     = PInt 0
                | otherwise = PInt (x' - y')

  x + y = PInt (fromPInt x + fromPInt y)

  x * y = PInt (fromPInt x * fromPInt y)

  abs x = x

  signum _ = 1

  fromInteger x = PInt (fromInteger x)

instance Eq PInt where
   x == y = fromPInt x == fromPInt y
   x /= y = fromPInt x /= fromPInt y

instance Ord PInt where
   x <= y = fromPInt x <= fromPInt y
   x < y         = fromPInt x < fromPInt y
   x > y         = fromPInt x > fromPInt y

instance Show PInt where
   show = show . fromPInt

toPInt :: Int -> PInt
toPInt x
  | x <= 0    = 0
  | otherwise = PInt x

