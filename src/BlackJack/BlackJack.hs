module BlackJack.BlackJack 
  ( toHand
  , startDeck
  , shuffledDeck
  , startMoney
  ) where

import BasicPrelude

import System.Random.Shuffle (shuffle')
import Data.Text hiding (length)
import Text.Read
import System.Random (newStdGen)

import Types

toHand :: Suit -> Hand
toHand suit =
  [ (Card Ace suit)
  , (Card King suit)
  , (Card Queen suit)
  , (Card Ace suit)
  , (Card (Pip 10) suit)
  , (Card (Pip 9) suit)
  , (Card (Pip 8) suit)
  , (Card (Pip 7) suit)
  , (Card (Pip 6) suit)
  , (Card (Pip 5) suit)
  , (Card (Pip 4) suit)
  , (Card (Pip 3) suit)
  , (Card (Pip 2) suit)
  ]

startDeck :: Hand
startDeck =
  (toHand Hearts) <> (toHand Clubs) <> (toHand Spades) <> (toHand Diamonds)

shuffledDeck :: IO Hand
shuffledDeck = do
  seed <- newStdGen
  return (shuffled seed)
  where
    shuffled seed = shuffle' startDeck (length startDeck) seed



