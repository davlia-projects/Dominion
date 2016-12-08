# Dominion #

## Files
* NOTE: Because of lazy IO, the game needs to be played in GHCi. Do not compile and run it as IO will be jumbled. *   
`Main.hs` - The Main file that inits and runs the game.  
`Tests.hs` - Unit tests and QC tests for major pure functions  
`Card/`  
  * `CardEffects.hs` - Contains all the card's IO and non-IO effects
  * `Cards.hs` - Contains all the cards in the game  
`Dominion/`
  * `Dominion.hs` - Contains all the library functions (Player transformers, GameState modifiers, getters, setters, etc.)
  * `DominionIO.hs`- Contains all the IO logic for the game and listens to user input
`lib/`
  * `DataTypes.hs` - Contains all the data structures of the game
  * `Utils.hs` - Contains all the utility functions that don't belong to any of the existing modules

Order of reading: `Main.hs` -> `DominionIO.hs` -> `DataTypes.hs` w/ `Cards.hs` -> `Dominion.hs` w/ `CardEffects.hs` -> the rest

## Overview ##
Game development has traditionally used C++ as its preferred language. Although there has been some simple games out there created with Haskell (https://wiki.haskell.org/Game_Development), I want to push Haskell to the limit. The overall goal is to recreate the multiplayer card game [Dominion](https://www.wikiwand.com/en/Dominion_(card_game)).

In the game, each player will take turns to build their deck by buying Kingdom cards, generating effects with Actions and earning Victory points. The game ends when a three piles of buyable Kingdom cards are depleted or the pile for highest Victory point card, the Province, is depleted.

Each card has its own unique effects, and thus different game mechanics emerge. Some additional game mechanics include:
Trashing cards
Discarding cards
Drawing cards
Making other players discard cards
Gain cards (different from buying cards)

## Typical Use Case ##
At a high level, the most typical use case is to have users play the game. The game is split into two portions, the server-side and the client-side. Each player would connect to a page where the game is and connect to the server. The game can be played with 2-4 players. As such, the game should accept I/O signals from each client. When the players are on the client, with enough players connected, the game will start. Each players are dealt an initial deck and starting buyable Kingdom cards will be randomly selected (might be changed depending on available time). Players will then take turns playing the game, going through each of their turn’s three phases. Once the game terminates, there will be a score screen showing each player’s final Victory points and their rankings.

## Components and Design ##
There are several components needed for this project:
* Game logic
    * Each card needs an effect and changes the current state of the game
    * The game should end properly and a winner should be determined
    * Players should only be able to play cards that they are able to
* I/O handling
    * Players should be able to choose what cards to play and resolve any effects when playing a card
    * Players should be able to choose to buy cards


At its core, our dominion game will have a Card, Player, and GameState data type that keeps track of all the ongoing interactions between players. The Card data type will keep track of details about the card and its effects when played. Each Player will keep track of their hand, deck, discard pile, action points, buying points, money, and identifiers. The GameState will keep track of the board in play, players, and any other relevant piles of cards that are relevant to the game.
