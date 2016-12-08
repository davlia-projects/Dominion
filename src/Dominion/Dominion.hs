module Dominion.Dominion where

import Data.List

import qualified Data.Map as M

import Lib.DataTypes
import Lib.Utils
import Card.Cards


-- Player information

{- ############### Game Init Functions ############### -}

initDeck :: [CardName]
initDeck = replicate 7 "copper" ++ replicate 3 "estate"

initPlayers :: [String] -> Players
initPlayers ps = M.fromList $ zip ps (zipWith initPlayer (iterate (+1) 0) ps)

initPlayer :: Int -> String -> Player
initPlayer pid n = drawFive (reshuffle (Player pid n initDeck [] [] 1 1 0))

initField :: [CardName] -> Int -> Field
initField cn x = M.fromList $ listField cn x
  where
    listField :: [CardName] -> Int -> [(CardName,Int)]
    listField (c:cs) x = 
      case c of
        "copper" -> ("copper", 60) : listField cs x
        "silver" -> ("silver", 40) : listField cs x
        "gold" -> ("gold", 30) : listField cs x
        "curse" -> case x of
                     1 -> ("curse", 10) : listField cs x
                     2 -> ("curse", 10) : listField cs x
                     3 -> ("curse", 20) : listField cs x
                     _ -> ("curse", 30) : listField cs x
        _ -> if hasType c Victory then
             case x of
               1 -> (c, 8) : listField cs x
               2 -> (c, 8) : listField cs x
               3 -> (c, 12) : listField cs x
               _ -> (c, 12) : listField cs x
             else (c, 10) : listField cs x
    listField _ _ = []

initGame :: [CardName] -> Players -> Int -> GameState
initGame cs ps = GameState num ps (initField cs num) 0 (head $ M.keys ps) []
  where
    num = length ps

{- #######################
   # Player Transformers #
   ####################### -}

draw :: Int -> Player -> Player
draw 0 p = p
draw i p = case deck p of
          [] -> draw i (reshuffle p)
          x:xs -> draw (i-1) p {deck = xs, hand = x : hand p}

-- Reshuffles the discard pile into the deck once the deck has run out of cards
reshuffle :: Player -> Player
reshuffle p = p {deck = shuffle remCards 1, discard = []}
  where
    remCards = deck p ++ discard p

discardCard :: CardName -> Player -> Player
discardCard = gainCard ? rmFromHand

discardCards :: [CardName] -> Player -> Player
discardCards cs p = foldr discardCard p cs

gainCard :: CardName -> Player -> Player
gainCard cn p = p {discard = cn : discard p}

rmFromHand :: CardName -> Player -> Player
rmFromHand cn p = p {hand = delete cn (hand p)}

rmFromHands :: [CardName] -> Player -> Player
rmFromHands cs p = foldr rmFromHand p cs

rmFromHandIf :: (CardName -> Bool) -> CardName -> Player -> Player
rmFromHandIf f cn p = if f cn then rmFromHand cn p else p

rmFromDiscard :: CardName -> Player -> Player
rmFromDiscard cn p = p {discard = delete cn (discard p)}

discardHand :: Player -> Player
discardHand p = p {discard = discard p ++ hand p, hand = []}

putBackCard :: CardName -> Player -> Player
putBackCard c p = (rmFromHand c p) {deck = c : deck p}

putBackCards :: [CardName] -> Player -> Player
putBackCards cs p = (rmFromHands cs p) {deck = cs ++ deck p}

drawFive :: Player -> Player
drawFive = draw 5

discardDeck :: Player -> Player
discardDeck p = p {discard = discard p ++ deck p, deck = []}

cleanupHand :: Player -> Player
cleanupHand = drawFive . discardHand

addTrash :: CardName -> GameState -> GameState
addTrash cn gs = gs {trash = cn : trash gs}

addMoney :: Int -> Player -> Player
addMoney n p = p {money = money p + n}

addAction :: Int -> Player -> Player
addAction n p = p {action = action p + n}

addBuy :: Int -> Player -> Player
addBuy n p = p {buy = buy p + n}

addToHand :: CardName -> Player -> Player
addToHand cn p = p {hand = cn : hand p}

hasCard :: CardName -> Player -> Bool
hasCard cn p = cn `elem` hand p

canAfford :: GameState -> CardName -> Bool
canAfford gs cn = cost (getCard' cn) <= money (getCurrentPlayer gs)

{- #######################
   # GameState Functions #
   ####################### -}

updatePlayer :: (Player -> Player) -> PlayerName -> GameState -> GameState
updatePlayer f pn gs = gs {players = M.adjust f pn (players gs)}

updatePlayers :: (Player -> Player) -> [PlayerName] -> GameState -> GameState
updatePlayers f pns gs = foldr (updatePlayer f) gs pns

updateCurrentPlayer :: (Player -> Player) -> GameState -> GameState
updateCurrentPlayer f gs = updatePlayer f (currentPlayer gs) gs

getCurrentPlayer :: GameState -> Player
getCurrentPlayer gs = getPlayer gs $ currentPlayer gs

getPlayer :: GameState -> PlayerName -> Player
getPlayer gs pn = players gs M.! pn

getPlayerNames :: GameState -> [PlayerName]
getPlayerNames gs = M.keys $ players gs

getOtherPlayersNames :: GameState -> [PlayerName]
getOtherPlayersNames gs = delete (currentPlayer gs) (getPlayerNames gs)

getPlayerHand :: GameState -> PlayerName -> [CardName]
getPlayerHand gs pn = hand $ getPlayer gs pn

getPlayerDiscard :: GameState -> PlayerName -> [CardName]
getPlayerDiscard gs pn = discard $ getPlayer gs pn

getPlayerMoney :: GameState -> PlayerName -> Int
getPlayerMoney gs pn = money $ getPlayer gs pn

getCard :: CardName -> Maybe Card
getCard cn = M.lookup cn allCards

getCard' :: CardName -> Card
getCard' = (M.!) allCards

getCardCost :: CardName -> Int
getCardCost = cost . getCard'

getCardTypes :: CardName -> [CardType]
getCardTypes cn = types (allCards M.! cn)

getCardEffect :: CardName -> [Effect]
getCardEffect cn = effect $ getCard' cn

filterType :: [CardType] -> [CardName] -> [CardName]
filterType ct = filter ((==) ct . getCardTypes)

getMoneyCards :: GameState -> [CardName]
getMoneyCards gs = filterType [Treasure] playerHand
  where
    playerHand = getPlayerHand gs (currentPlayer gs)

getFirstCard :: GameState -> CardName
getFirstCard gs = head $ hand $ getCurrentPlayer gs

getPlayerScore :: Player -> Int
getPlayerScore p = sum (map victoryPoints getAllCards) + numCards "gardens" * (length playerCards `div` 10)
  where
    playerCards = deck p ++ discard p ++ hand p
    getAllCards = map (allCards M.!) playerCards
    numCards cn = length (filter (== cn) playerCards)

getPlayerScore' :: GameState -> PlayerName -> Int
getPlayerScore' gs p = getPlayerScore $ getPlayer gs p

hasTypes :: CardName -> [CardType] -> Bool
hasTypes cn = all $ hasType cn

hasType :: CardName -> CardType -> Bool
hasType cn t = t `elem` types (getCard' cn)

trashCard :: CardName -> GameState -> GameState
trashCard cn gs = updateCurrentPlayer (rmFromHand cn) (addTrash cn gs)

trashCard' :: CardName -> PlayerName -> GameState -> GameState
trashCard' cn pn gs = updatePlayer (rmFromHand cn) pn (addTrash cn gs)

isGameOver :: GameState -> Bool
isGameOver gs = provinceGone gs || pilesGone gs
  where
    provinceGone gs' = (field gs') M.! "province" == 0
    pilesGone gs' = length (filter (== 0) (M.elems (field gs'))) >= 3

buyCard :: CardName -> GameState -> GameState
buyCard cn gs = updateCurrentPlayer (update . gainCard cn) gs'
  where
    c = allCards M.! cn
    update p = p {buy = buy p - 1, money = money p - cost c}
    gs' = gs {field = M.adjust (+(-1)) cn (field gs)}

{- ##############
   # VALIDATION #
   ############## -}

ensureCardInHand :: Validation
ensureCardInHand gs cn = hasCard cn (getCurrentPlayer gs)

ensureCardInHand' :: PlayerName -> Validation
ensureCardInHand' p gs cn = hasCard cn (getPlayer gs p)

ensureDiscard :: Validation
ensureDiscard = ensureCardInHand

ensureDiscard' :: PlayerName -> Validation
ensureDiscard' = ensureCardInHand'

ensureTrash :: Validation
ensureTrash = ensureCardInHand

ensureTrash' :: PlayerName -> Validation
ensureTrash' = ensureCardInHand'

ensureIsTreasure :: Validation
ensureIsTreasure _ cn = Treasure `elem` types (getCard' cn)

ensureIsVictory :: Validation
ensureIsVictory _ cn = Victory `elem` types (getCard' cn)

ensureNotVictory :: Validation
ensureNotVictory _ cn = [Victory] /= getCardTypes cn

ensureConstraints :: [Validation] -> Validation
ensureConstraints vals gs cn = all (\f -> f gs cn) vals

ensureCost :: Int -> Validation
ensureCost n _ cn = getCardCost cn <= n

ensureIsOnField :: Validation
ensureIsOnField gs cn = M.member cn (field gs) && field gs M.! cn > 0

ensureGain :: Int -> Validation
ensureGain n = ensureConstraints [ensureIsOnField, ensureCost n]

ensurePlay :: Validation
ensurePlay = ensureConstraints [ensureCardInHand, ensureNotVictory]

ensureBuy :: Validation
ensureBuy gs cn = ensureGain (getCardCost cn) gs cn &&
                    money p >= 0 && buy p >= 0
  where
    tryBuy = buyCard cn gs
    p = players tryBuy M.! currentPlayer tryBuy
    -- nonNeg f = all (>= 0) (M.elems f)

allowBack :: Validation -> Validation
allowBack val gs cn = val gs cn || cn == "back"

{- #############
   # NEXT TURN #
   ############# -}
setNextCurrentPlayer :: GameState -> GameState
setNextCurrentPlayer gs = gs {currentPlayer = nextPlayer, players = newPlayers}
  where
    nextPlayer = getNextPlayer gs (currentPlayer gs)
    newPlayers = M.adjust (\f -> f {action = 1, buy = 1, money = 0}) nextPlayer (players gs)

getNextPlayer :: GameState -> PlayerName -> PlayerName
getNextPlayer gs = nextPlayer allPlayers
  where
    allPlayers = M.keys $ players gs
    nextPlayer (x1:x2:xs) p
      | p == x1   = x2
      | otherwise = nextPlayer (x2:xs) p
    nextPlayer [x1] p = nextPlayer (x1:allPlayers) p
    nextPlayer _ _    = undefined --TODO do some error handling?

isTurnOver :: GameState -> Bool
isTurnOver gs = buy player == 0 && action player == 0
  where
    player =  players gs M.! currentPlayer gs

endTurn :: GameState -> GameState
endTurn gs = gs {players = M.adjust finish (currentPlayer gs) (players gs)}
  where
    finish p = p {buy = 0, action = 0, money = 0}

advanceTurn :: GameState -> GameState
advanceTurn gs = if isTurnOver gs
                 then (setNextCurrentPlayer . endTurn) cleanedUp
                 else gs
  where
    cleanedUp = updateCurrentPlayer cleanupHand gs
