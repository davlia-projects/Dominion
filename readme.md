# Dominion #
By: Zhan Chin Xiong (chinz), Anthony Hsieh (hsieha), David Liao (liaod)

## Files
* NOTE: Because of lazy IO, we require the game be played in GHCi. Do not compile and run it as IO will be jumbled. *   
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

## Overall Goal ##
Game development has traditionally used C++ as its preferred language. Although there has been some simple games out there created with Haskell (https://wiki.haskell.org/Game_Development), we want to push Haskell to the limit. Our overall goal is to recreate the multiplayer card game [Dominion](https://www.wikiwand.com/en/Dominion_(card_game)).

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
* Networking
    * We would like the option to play over the network with multiple clients

At its core, our dominion game will have a Card, Player, and GameState data type that keeps track of all the ongoing interactions between players. The Card data type will keep track of details about the card and its effects when played. Each Player will keep track of their hand, deck, discard pile, action points, buying points, money, and identifiers. The GameState will keep track of the board in play, players, and any other relevant piles of cards that are relevant to the game.

There will be a server-side and client-side to the project. The server side will handle all game logic and all game transitions. It will serve to do logic validation on the game (if it is the Buy phase, you can’t play a Action card). It receives Network IO from sockets connecting to each client in the form of a JSON storing information such as a player’s card choice or decision for an action. Every time the GameState is updated on the server side, the server will emit the game state as a JSON to the client-side and each client will determine how to render the game state.

The client side does no game logic calculation and only serves to allow certain actions depending on the phase and render the game state. There will be event handling to allow for certain actions to be completed (user clicks). The game state should be rendered through template generated javascript. The decoupled design of the server and client sides allows us to work towards two major checkpoints. If in any instance the server-side (which is prioritized over the client-side) design takes more effort than anticipated, we could have a simpler client-side.

## Testing ##
For this project, testing will primarily be focused on the game logic. The game state will be under many constraints following the rules of the game. We can unit test all of these constraints (e.g. bounds, score) for each card given a specific game state. Some of the constraints can be tested using QuickCheck as well. For example, we can test our shuffling function by generating different decks and making sure that after shuffling, certain properties still hold. This will be very handy for regression testing any changes or new features. Because some card effects depend on inputs from the player, we will have some test cases where user input is needed, and we will verify that none of the constraints break. The server side will have its own set of tests that verify game logic using dummy client inputs. The client side will have dummy GameStates to verify correct rendering.

## Effort Budget ##
Most of the components can be done in parallel such as the network IO and game logic. The network IO portion can simulated  and tested using code dummies and stubs while the game logic can be completed and tested using unit tests and quick check. Because of this modular design, there are two step-by-step processes in parallel. Each step will be followed by testing.

### Server side
* Plan out and implement all fundamental data types (player, board, cards, etc.) 5 hr
* Implement player actions 15 hr
* Handle IO >15 hr
* Game validation 5 hr
* Implement scoring and game states 6 hr

### Network
* Handle concurrent clients 5 hr
* Syncing and event handling 5 hr

### Client Side
* Rendering GUI and Game State 15 hr
* Client side event handling (clicks and drags) 10 hr
* Putting it all together
* Final testing (Both white-box and black-box tests) 6 hr
* Refactor + beautifying code 4 hr
* Documentation 3 hr

## Re-designs ##
* Hard coding Card abilities does not work for unique effects.
    * Fix was to add in core effects and then functional effects (Player -> Player).
* Player Transformers (Player -> Player) were generalized to all functions in Dominion function library
    * Enabled application through `updatePlayer` and `updateCurrentPlayer` using these transformers
* A dependency cycle formed which required us to reroute Cards to not depend on CardEffects. ![dependency issue](http://puu.sh/lKsJy/e3e186881f.png)
* Refactored a lot of effects into very generic templates. Most cards and new cards will be very easily written following these template functions
* Validating user input designed like a decorator for functions that query users for input.
