module Card.Cards where

import Data.Map

import Lib.DataTypes

allCards :: Cards
allCards = baseCards `union` kingdomCards

allCardNames :: [CardName]
allCardNames = keys allCards

baseCards :: Cards
baseCards = fromList [ ("copper", copper)
                    , ("silver", silver)
                    , ("gold", gold)
                    , ("curse", curse)
                    , ("estate", estate)
                    , ("duchy", duchy)
                    , ("province", province) ]

kingdomCards :: Cards
kingdomCards = fromList [ ("cellar", cellar)
                        , ("chapel", chapel)
                        , ("moat", moat)
                        , ("village", village)
                        , ("chancellor", chancellor)
                        , ("woodcutter", woodcutter)
                        , ("workshop", workshop)
                        , ("bureaucrat", bureaucrat)
                        , ("feast", feast)
                        , ("militia", militia)
                        , ("moneylender", moneylender)
                        , ("remodel", remodel)
                        , ("smithy", smithy)
                        , ("spy", spy)
                        , ("thief", thief)
                        , ("throneroom", throneroom)
                        , ("councilroom", councilroom)
                        , ("festival", festival)
                        , ("laboratory", laboratory)
                        , ("library", library)
                        , ("mine", mine)
                        , ("market", market)
                        , ("witch", witch)
                        , ("adventurer", adventurer) ]

copper :: Card
copper = Card "Copper" [Treasure] 0 0 [PlusMoney 1] "+$1."

silver :: Card
silver = Card "Silver" [Treasure] 3 0 [PlusMoney 2] "+$2."

gold :: Card
gold = Card "Gold" [Treasure] 6 0 [PlusMoney 3] "+$3."

curse :: Card
curse = Card "Curse" [Victory] 0 (-1) [] "-1 Victory Point."

estate :: Card
estate = Card "Estate" [Victory] 2 0 [] "+1 Victory Point."

duchy :: Card
duchy = Card "Duchy" [Victory] 5 0 [] "+3 Victory Point."

province :: Card
province = Card "Province" [Victory] 8 0 [] "+6 Victory Point."

-- Original Kingdom Set
cellar :: Card
cellar = Card "Cellar" [Action] 2 0 [PlusAction 1, IOEffect "cellar"] "+1 Action. Discard any number of cards. +1 Card per card discarded."

chapel :: Card
chapel = Card "Chapel" [Action] 2 0 [IOEffect "chapel"] "Trash up to 4 cards from your hand."

moat :: Card
moat = Card "Moat" [Action, Reaction] 2 0 [PlusCard 2] "+2 Cards. When another player plays an Attack card, you may reveal this from your hand. If you do, you are unaffected by that Attack."

chancellor :: Card
chancellor = Card "Chancellor" [Action] 3 0 [PlusMoney 2, IOEffect "chancellor"] "+$2. You may discard your entire deck."

village :: Card
village = Card "Village" [Action] 3 0 [PlusCard 1, PlusAction 2] "+1 Card. +2 Actions."

woodcutter :: Card
woodcutter = Card "Woodcutter" [Action] 3 0 [PlusBuy 1, PlusMoney 2] "+1 Buy. +$2."

workshop :: Card
workshop = Card "Workshop" [Action] 3 0 [IOEffect "workshop"] "Gain a card costing up to $4."

bureaucrat :: Card
bureaucrat = Card "Bureaucrat" [Action, Attack] 4 0 [IOEffect "bureaucrat"] "Gain a Silver card; put it on top of your deck. Each other player reveals a Victory card from his hand and puts it on his deck (or reveals a hand with no Victory cards)."

feast :: Card
feast = Card "Feast" [Action] 4 0 [IOEffect "feast"] "Trash this card. Gain a card costing up to $5."

gardens :: Card
gardens = Card "Gardens" [Victory] 4 0 [] "Worth 1 Victory for every 10 cards in your deck."

militia :: Card
militia = Card "Militia" [Action, Attack] 4 0 [PlusMoney 2, IOEffect "militia"] "+$2. Each other player discards down to 3 cards in his hand." 

moneylender :: Card
moneylender = Card "Moneylender" [Action] 4 0 [IOEffect "moneylender"] "Trash a Copper and gain +$3."

remodel :: Card
remodel = Card "Remodel" [Action] 4 0 [IOEffect "remodel"] "Trash a card and gain a card up to $2 more than the trashed card."

smithy :: Card
smithy = Card "Smithy" [Action] 4 0 [PlusCard 3] "+3 Cards."

spy :: Card
spy = Card "Spy" [Action, Attack] 4 0 [PlusCard 1, PlusAction 1, IOEffect "spy"] "+1 Card. +1 Action. Each player (including you) reveals the top card of his deck and either discards it or puts it back, your choice."

thief :: Card
thief = Card "Thief" [Action, Attack] 4 0 [IOEffect "thief"] "Each other player reveals the top 2 cards of his deck. If they revealed any Treasure cards, they trash one of them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards."

throneroom :: Card
throneroom = Card "Throne Room" [Action] 4 0 [IOEffect "throneroom"] "Play an Action card twice."

councilroom :: Card
councilroom = Card "Council Room" [Action] 5 0 [PlusCard 4, PlusBuy 1, Effect "councilroom"] "+4 Cards. +1 Buy. Each other player draws a card."

festival :: Card
festival = Card "Festival" [Action] 5 0 [PlusAction 2, PlusBuy 1, PlusMoney 2] "+2 Actions. +1 Buy. +$2."

laboratory :: Card
laboratory = Card "Laboratory" [Action] 5 0 [PlusCard 2, PlusAction 1] "+2 Cards. +1 Action."

library :: Card
library = Card "Library" [Action] 5 0 [IOEffect "library"] "Draw until you have 7 cards in hand. You may discard any Action cards along the way."

market :: Card
market = Card "Market" [Action] 5 0 [PlusCard 1, PlusAction 1, PlusBuy 1, PlusMoney 1] "+1 Card. +1 Action. +1 Buy. +$1."

mine :: Card
mine = Card "Mine" [Action] 5 0 [IOEffect "mine"] "Trash a Treasure card and gain a Treasure card up to $3 more; put it into your hand."

witch :: Card
witch = Card "Witch" [Action, Attack] 5 0 [PlusCard 2, IOEffect "witch"] "+2 Cards. Other players gains a Curse card."

adventurer :: Card
adventurer = Card "Adventurer" [Action] 6 0 [IOEffect "adventurer"] "Reveal cards from your deck until you reveal 2 Treasure cards. Put those Treasure cards in your hand and discard the other revealed cards."
