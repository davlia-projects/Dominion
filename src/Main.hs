{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Main where

import Control.Monad.State
import System.Random

import Lib.DataTypes
import Dominion.Dominion
import Dominion.DominionIO
import Card.Cards

pollForPlayers :: [PlayerName] -> IO [PlayerName]
pollForPlayers pns = do
  putStr "Enter new player name or say 'start' to begin: "
  line <- getLine
  case line of
    "start" -> if (not . null) pns
               then return pns
               else do
                 putStrLn "You need more players!"
                 pollForPlayers pns
    name    -> if name `elem` pns
               then do
                 putStrLn "Sorry this name is already taken!"
                 pollForPlayers pns
               else pollForPlayers (name : pns)

main :: IO ()
main = do
  randomSeed <- randomIO
  putStrLn "Welcome to Dominion (adapted from the game by Donald X. Vaccarino)!"
  putStrLn "Created by Anthony Hsieh, David Liao, Zhan Chin"
  putStrLn instructions
  playerNames <- pollForPlayers []
  let gamePlayers = initPlayers playerNames
  let gameState = initGame allCardNames gamePlayers randomSeed
  _ <- execStateT gameLoop gameState
  putStrLn "Thank you for playing!"

-- Dummy testing main
testPlayers :: [PlayerName]
testPlayers = ["David"]
