module Lib.DataTypes where

import Data.Map (Map)

import Control.Monad.State

-- The primary game state
data GameState = GameState { numPlayers :: Int
                           , players :: Players
                           , field :: Field
                           , numTurn :: Int
                           , currentPlayer :: PlayerName
                           , trash :: [CardName]
                           , seed :: Int
                           } deriving (Show, Eq)

data Player = Player { playerID :: Int
                     , playerName :: String
                     , deck :: [CardName]
                     , discard :: [CardName]
                     , hand :: [CardName]
                     , action :: Int
                     , buy :: Int
                     , money :: Int
                     } deriving (Eq)

data Card = Card { cardName :: String
                 , types :: [CardType]
                 , cost :: Int
                 , victoryPoints :: Int
                 , effect :: [Effect]
                 , description :: String
                 }

data Effect = PlusAction Int
            | PlusBuy Int
            | PlusMoney Int
            | PlusCard Int
            | Effect EffectName
            | IOEffect EffectName

listToString :: Show a => (Player -> [a]) -> Player -> String
listToString f p = go $ f p
  where
    go (c:cs) = show c ++ " " ++ go cs
    go []     = "\n"

instance Show Player where
  show p = "Player: " ++ playerName p ++ "\n" ++
           "Hand: " ++ listToString hand p ++
           "Actions: " ++ show (action p) ++
           " Buy: " ++ show (buy p) ++
           " Money: " ++ show (money p) ++ "\n"

instance Eq Card where
  c1 == c2 = cardName c1 == cardName c2

instance Show Card where
  show Card {cardName = n} = show n

instance Ord Card where
  (Card {cardName = n1}) `compare` (Card {cardName = n2}) = n1 `compare` n2

data CardType = Treasure
              | Victory
              | Action
              | Attack
              | Reaction
              | Curse
  deriving (Eq, Show)

instance Show Effect where
  show (Effect s) = "Effect: " ++ s
  show (IOEffect s) = "IOEffect: " ++ s
  show (PlusAction i) = "PlusAction " ++ show i
  show (PlusBuy i) = "PlusBuy " ++ show i
  show (PlusMoney i) = "PlusMoney " ++ show i
  show (PlusCard i) = "PlusCard " ++ show i

-- Keeps track of all the cards remaining on the field
type Field = Map CardName Int

type Players = Map String Player

type IOEffects = Map String (GameState -> IO GameState)

type Effects = Map String (GameState -> GameState)

type PlayerName = String

type EffectName = String

type CardName = String

type Cards = Map String Card

type Validation = GameState -> CardName -> Bool

type GameStateIO a = StateT GameState IO a
