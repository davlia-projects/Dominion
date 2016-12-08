module Dominion.DominionIO where

import Lib.DataTypes
import Card.CardEffects
import Dominion.Dominion
import Data.List

import Control.Monad.State

instructions :: String
instructions = "How to Play: \n" ++
               "Try to get as many Victory points as possible! \n" ++
               "The game will end when there are no more provinces or 3 piles run out. \n" ++
               "play: Choose a card from hand to play. Select back to go back. \n" ++
               "buy: Choose a card to buy. Select back to go back. \n" ++
               "money: Automatically plays all of your money. \n" ++
               "moreinfo: Displays additional information about your status. \n" ++
               "kingdom: Displays all cards in the kingdom. \n" ++
               "help: Dispays the instructions. \n" ++
               "end: Ends your turn."

gameOver :: GameStateIO ()
gameOver = do
  lift $ putStrLn "Game Over!"
  gs <- get
  let score = map (getPlayerScore' gs) (getPlayerNames gs)
      l = sortBy sortGT $ zip (getPlayerNames gs) score
  lift $ putStrLn (show l)
  where
    sortGT (a1, b1) (a2, b2)
      | b1 < b2 = GT
      | b1 > b2 = LT
      | b1 == b2 = compare a1 a2    

liftPut :: IO GameState -> GameStateIO ()
liftPut p = lift p >>= put

listenTo :: String -> GameStateIO ()
listenTo "play" = handleAction
listenTo "buy" = handleBuy
listenTo "money" = handleMoney
listenTo "moreinfo" = handleMoreInfo
listenTo "kingdom" = handleKingdom
listenTo "help" = handleHelp
listenTo "end" = handleEnd
listenTo _ = handleInvalid

handleBuy :: GameStateIO ()
handleBuy = do
  gs <- get
  if buy (getCurrentPlayer gs) > 0
  then do
    lift $ putStrLn "Here is what you can buy:\n"
    lift $ displayField canAfford gs
    card <- lift $ queryUserCard (allowBack ensureBuy) gs
    unless (card == "back") $ modify $ buyCard card
  else lift $ putStrLn "You can't buy anymore cards!"

handleAction :: GameStateIO ()
handleAction = do
  gs <- get
  if action (getCurrentPlayer gs) > 0
  then do
    lift $ displayHand gs
    card <- lift $ queryUserCard (allowBack ensurePlay) gs
    unless (card == "back") $ liftPut $ playCard card gs
  else lift $ putStrLn "You can't play anymore action cards!"

handleMoney :: GameStateIO ()
handleMoney = do
  gs <- get
  liftPut $ playCards (getMoneyCards gs) gs

handleMoreInfo :: GameStateIO ()
handleMoreInfo = do
  gs <- get
  lift $ putStr  $ "Discard: " ++ listToString discard (getCurrentPlayer gs)

handleKingdom :: GameStateIO ()
handleKingdom = do
  gs <- get
  lift $ displayAllField gs

handleHelp :: GameStateIO ()
handleHelp = do
  lift $ putStrLn instructions

handleEnd :: GameStateIO ()
handleEnd = do
  lift $ putStrLn "ending turn"
  modify endTurn

handleInvalid :: GameStateIO ()
handleInvalid = lift $ putStrLn "Not a valid option"

playerTurn :: GameStateIO ()
playerTurn = do
  gs <- get
  lift $ putStrLn ""
  lift $ print (getCurrentPlayer gs)
  lift $ putStr "Choose an action (play, buy, money, moreinfo, kingdom, help, end): "
  userAction <- lift getLine
  listenTo userAction
  modify advanceTurn
  gs' <- get
  if currentPlayer gs == currentPlayer gs' 
  then do
    playerTurn
  else do
    return ()

gameLoop :: GameStateIO ()
gameLoop = do
  gs <- get
  if isGameOver gs
  then gameOver
  else do
    _ <- playerTurn
    gameLoop
