{-# LANGUAGE RecordWildCards #-}

module Tests where

import Card.Cards as C
import Card.CardEffects as CE
import Dominion.Dominion as D
import Dominion.DominionIO as DIO
import Lib.DataTypes as DT
import Lib.Utils as U

import Test.QuickCheck as QC hiding (discard)
import Test.QuickCheck.Monadic
import Data.List hiding (insert)
import Data.Map hiding (null, map, filter)
import Test.HUnit hiding (assert)
import System.Random
import Control.Monad

doTests :: IO ()
doTests = do
  _ <- runTestTT $ TestList [ testDraw
                            , testGetMoneyCards
                            , testGetPlayerLandScore
                            , testSetNextCurrentPlayer
                            , testEndTurn
                            , testCouncilRoom
                            ]
  return ()

doProps :: IO ()
doProps = do
  putStrLn "Dominion Library Functions"
  qcName "draw" prop_draw1
  qcName "discard1" prop_discard1
  qcName "discard2" prop_discard2
  qcName "gardens" prop_gardens1
  qcName "buy" prop_buy1
  qcName "councilroom" prop_councilroom
  qcName "witch" prop_witch
  qcName "adventurer" prop_adventurer

  where
    qcName name prop = do
      putStr $ name ++ ": "
      quickCheck prop

main :: IO ()
main = do
  doTests
  doProps
  return ()

instance Arbitrary Player where
  arbitrary = do
    pid <- arbitrary
    pn <- arbitrary
    deck' <- sublistOf allCardNames
    disc <- sublistOf allCardNames
    hand' <- sublistOf allCardNames
    act <- choose (0,100)
    buy' <- choose (0,100)
    mon <- choose (0,100)
    return $ Player pid pn deck' disc hand' act buy' mon
  shrink = shrink

instance Arbitrary GameState where
  arbitrary = do
    np <- choose (1, 5)
    randNames <- replicateM np $ elements ["David", "Zhan", "Anthony", "Player1", "Player2"]
    randPlayers <- replicateM np arbitrary
    let ps = fromList $ map (\(n,p) -> (n, p {playerName = n})) (zip randNames randPlayers)
    randCards <- sublistOf allCardNames
    randNums <- replicateM (length randCards) (choose (1, 25))
    let f = fromList $ zip randCards randNums
    return $ GameState np ps f 0 (head $ keys ps) [] 1

-- Constraint modifiers for our QC
newtype CardMod = CardMod CardName deriving (Show, Eq)

instance Arbitrary CardMod where
  arbitrary = elements (map CardMod allCardNames)

p1 :: Player
p1 = Player { playerID = 0
            , playerName = "p1"
            , deck = []
            , discard = []
            , hand = []
            , action = 1
            , buy = 1
            , money = 0
            }

p1' :: Player
p1' = p1 {deck = initDeck}

gs1 :: GameState
gs1 = GameState 1 (fromList [("p1", p1)]) (initField allCardNames) 0 "p1" [] 1

gs1' :: GameState
gs1' = gs1 {players = insert "p1" p1' (players gs1)}

testDraw :: Test
testDraw = TestList
  [ draw 0 p1 ~?= p1
  , draw 1 p1 ~?= p1
  , deck dp1 ~?= ["copper", "copper", "estate", "estate", "estate"]
  , hand dp1 ~?= replicate 5 "copper"
  , deck dp2 ~?= []
  , hand dp2 ~?= replicate 3 "copper"
  , deck dp3 ~?= ["estate", "estate"]
  , hand dp3 ~?= ["estate", "estate", "copper", "copper", "copper"]
  ]
  where
    dp1 = draw 5 p1'
    dp2 = draw 5 (p1 {deck = ["copper", "copper", "copper"]})
    dp3 = draw 5 (p1 {deck = ["copper", "copper", "copper"], discard = ["estate", "estate", "estate", "estate"]})

-- Drawing cards should not remove or add cards
prop_draw1 :: Player -> Int -> Bool
prop_draw1 p n = length (hand p ++ deck p ++ discard p) == length (hand dp ++ deck dp ++ discard dp)
  where
    dp = draw n p

prop_discard1 :: CardMod -> Player -> Property
prop_discard1 (CardMod cn) p = cn `elem` hand p ==> length (hand p') < length (hand p)
  where
    p' = discardCard cn p

prop_discard2 :: CardMod -> Player -> Property
prop_discard2 (CardMod cn) p = cn `elem` hand p ==> length (discard p') > length (discard p)
  where
    p' = discardCard cn p

testGetMoneyCards :: Test
testGetMoneyCards = TestList
  [ getMoneyCards gs1 ~?= []
  , getMoneyCards (genGS gs1 newp1) ~?= ["copper", "copper", "copper"]
  , getMoneyCards (genGS gs1 newp2) ~?= []
  ]
  where
    newp1 = p1 {hand = ["copper", "copper", "copper"]}
    newp2 = p1 {hand = ["estate", "estate", "estate"]}
    genGS gs newp = gs {players = insert "p1" newp (players gs)}

testGetPlayerLandScore :: Test
testGetPlayerLandScore = TestList
  [ getPlayerLandScore p1' ~?= 3
  , getPlayerLandScore p1 ~?= 0
  , getPlayerLandScore (p1 {hand = ["estate"]}) ~?= 1
  , getPlayerLandScore (p1 {deck = ["estate"]}) ~?= 1
  , getPlayerLandScore (p1 {discard = ["estate"]}) ~?= 1
  , getPlayerLandScore (p1 {hand = ["duchy"]}) ~?= 3
  , getPlayerLandScore (p1 {deck = ["duchy"]}) ~?= 3
  , getPlayerLandScore (p1 {discard = ["duchy"]}) ~?= 3
  , getPlayerLandScore (p1 {hand = ["province"]}) ~?= 6
  , getPlayerLandScore (p1 {deck = ["province"]}) ~?= 6
  , getPlayerLandScore (p1 {discard = ["province"]}) ~?= 6
  ]


-- Gardens score is 1 per 10 cards rounded down, gardens included
prop_gardens1 :: Int -> Int -> Property
prop_gardens1 total ng = total >= ng && ng >= 0 ==>
  getGardensScore newp1 == (total `div` 10) * ng
  where
    newp1 = p1 {deck = replicate (total - ng) "someCard" ++ replicate ng "gardens"}

prop_buy1 :: CardMod -> GameState -> Property
prop_buy1 (CardMod cn) gs = member cn (field gs) ==>
                       buy bcp == buy cp - 1
                    && money bcp == money cp - getCardCost cn
                    && field bought ! cn == field gs ! cn - 1
                    && length (getPlayerCards bcp) == length (getPlayerCards cp) + 1
  where
    bought = buyCard cn gs
    bcp = getPlayer bought $ currentPlayer gs
    cp = getPlayer gs $ currentPlayer gs


testSetNextCurrentPlayer :: Test
testSetNextCurrentPlayer = TestList
  [ currentPlayer nextSet1 ~?= "p1"
  , action (getCurrentPlayer nextSet1) ~?= 1
  , buy (getCurrentPlayer nextSet1) ~?= 1
  , money (getCurrentPlayer nextSet1) ~?= 0
  , currentPlayer nextSet2 ~?= "p2"
  , action (getCurrentPlayer nextSet2) ~?= 1
  , buy (getCurrentPlayer nextSet2) ~?= 1
  , money (getCurrentPlayer nextSet2) ~?= 0
  , currentPlayer nextSet3 ~?= "p1"
  , action (getCurrentPlayer nextSet3) ~?= 1
  , buy (getCurrentPlayer nextSet3) ~?= 1
  , money (getCurrentPlayer nextSet3) ~?= 0
  ]
  where
    nextSet1 = setNextCurrentPlayer gs1
    nextSet2 = setNextCurrentPlayer (gs1 {players = insert "p2" p1 (players gs1)})
    nextSet3 = setNextCurrentPlayer (nextSet2 {currentPlayer = "p2"})

testAdvanceTurn :: Test
testAdvanceTurn = TestList
  [ advanceTurn gs1 ~?= gs1'
  , advanceTurn gs1' ~?= gs1'
  , advanceTurn gs2 ~?= (setNextCurrentPlayer . endTurn) gs2
  , advanceTurn gs2' ~?=  gs2'
  ]
  where
    gs1' = updateCurrentPlayer (\p -> p {buy = 1}) gs1
    gs2 = gs1 {players = insert "p2" p1 (players gs1)}
    gs2' = updatePlayer (\p -> p {buy = 0, action = 0}) "p2" gs2

testEndTurn :: Test
testEndTurn = TestList
  [ endTurn gs1 ~?= updateCurrentPlayer (\p -> p {buy = 0, action = 0}) gs1
  , buy currentEnded ~?= 0
  , action currentEnded ~?= 0
  , money currentEnded ~?= 0
  ]
  where
    ended1 = endTurn gs1'
    currentEnded = getCurrentPlayer ended1

-- Card Effects

testCouncilRoom :: Test
testCouncilRoom = TestList
  [ cp ~?= getCurrentPlayer gs1
  , cp' ~?= getCurrentPlayer gs1'
  , length (getPlayerHand gs2' "p2") ~?= length (getPlayerHand gs2 "p2") + 1
  ]
  where
    cp = getCurrentPlayer (councilroomEffect gs1)
    cp' = getCurrentPlayer (councilroomEffect gs1')
    gs2 = gs1 {players = insert "p2" p1' (players gs1)}
    gs2' = councilroomEffect gs2

-- all other players gain a card
prop_councilroom :: GameState -> Bool
prop_councilroom gs = all prop otherPlayers
  where
    otherPlayers = getOtherPlayersNames gs
    gs' = councilroomEffect gs
    prop p = length (getPlayerHand gs p) + 1 == length (getPlayerHand gs' p)

-- all other players gain a curse
prop_witch :: GameState -> Bool
prop_witch gs = all prop otherPlayers
  where
    otherPlayers = getOtherPlayersNames gs
    gs' = witchEffect gs
    prop p = length (getCurses gs p) + 1 == length (getCurses gs' p)
    getCurses g = filter (== "curse") . getPlayerDiscard g

-- you should have at most 2 more treasures than before
prop_adventurer :: GameState -> Bool
prop_adventurer gs = prop
  where
    prop = length (getMoneyCards gs') - length (getMoneyCards gs)<= 2
    gs' = adventurerEffect gs
