module Card.CardEffects where

import Control.Monad
import qualified Data.Map as M
import Data.List
import Text.PrettyPrint

import Lib.DataTypes
import Dominion.Dominion
import Lib.Utils

displayCards :: Show a => [a] -> IO ()
displayCards (c:cs) = do
  putStr $ show c ++ " "
  displayCards cs
displayCards [] = putStr "\n"

displayCard :: Field -> CardName -> IO ()
displayCard f cn = putStrLn $ render doc ++ "\n"
  where
    doc = text "Card:" <+> text (show $ cardName c) <+>
          text "Cost:" <+> text (show $ cost c) <+>
          text "Remaining:" <+> text (show $ f M.! cn) $$
          text "Description:" <+> text (description c)
    c = getCard' cn

displayAllField :: GameState -> IO ()
displayAllField = displayField (\_ _ -> True)

displayField :: (GameState -> CardName -> Bool) -> GameState -> IO ()
displayField f gs = mapM_ (displayCard $ field gs) filtered
  where
    filtered = filter (f gs) (M.keys $ field gs)

displayHand :: GameState -> IO ()
displayHand gs = do
  putStrLn "Here are your cards: "
  displayCards $ getPlayerHand gs (currentPlayer gs)

displayHand' :: PlayerName -> GameState -> IO ()
displayHand' p gs = do
  putStrLn "Here are your cards: "
  displayCards $ getPlayerHand gs p

playCards :: [CardName] -> GameState -> IO GameState
playCards cns gs = foldM (flip playCard) gs cns

playCard :: CardName -> GameState -> IO GameState
playCard cn gs = applyEffects (effect c) (updateCurrentPlayer (transP cn) gs)
  where
    c = getCard' cn
    transP cn' p = case types (getCard' cn') of
              [Treasure] -> discardCard cn' p
              _          -> (discardCard cn' p) { action = action p - 1}

applyEffects :: [Effect] -> GameState -> IO GameState
applyEffects es p = foldM (flip applyEffect) p es

applyEffect :: Effect -> GameState -> IO GameState
applyEffect e@(IOEffect _) p = applyIOEffect e p
applyEffect e@_            p = return $ applyDetEffect e p

getIOEffect :: EffectName -> GameState -> IO GameState
getIOEffect en = allIOEffects M.! en

getEffect :: EffectName -> GameState -> GameState
getEffect en = allDetEffects M.! en

applyDetEffect :: Effect -> GameState -> GameState
applyDetEffect (PlusAction x) gs = updateCurrentPlayer (addAction x) gs
applyDetEffect (PlusBuy x)    gs = updateCurrentPlayer (addBuy x) gs
applyDetEffect (PlusMoney x)  gs = updateCurrentPlayer (addMoney x) gs
applyDetEffect (PlusCard x)   gs = updateCurrentPlayer (draw x) gs
applyDetEffect (Effect s)     gs = getEffect s gs
applyDetEffect _              gs = gs

applyIOEffect :: Effect -> GameState -> IO GameState
applyIOEffect (IOEffect s) p = getIOEffect s p
applyIOEffect _            p = return p

allIOEffects :: IOEffects
allIOEffects = M.fromList [ ("cellar", cellarEffect)
                          , ("chapel", chapelEffect)
                          , ("chancellor", chancellorEffect)
                          , ("workshop", workshopEffect)
                          , ("bureaucrat", bureaucratEffect)
                          , ("feast", feastEffect)
                          , ("militia", militiaEffect)
                          , ("moneylender", moneylenderEffect)
                          , ("remodel", remodelEffect)
                          , ("spy", spyEffect)
                          , ("thief", thiefEffect)
                          , ("throneroom", throneroomEffect)
                          , ("library", libraryEffect)
                          , ("mine", mineEffect)
                          , ("witch", witchEffect)
                           ]

allDetEffects :: Effects
allDetEffects = M.fromList [ ("councilroom", councilroomEffect)
                           , ("adventurer", adventurerEffect) ]

-- Takes in a validation function and asks for user input
queryUserCard :: Validation -> GameState -> IO CardName
queryUserCard f gs = do
  putStr "Pick a card: "
  c <- getLine
  if f gs c then return c else do
    putStrLn "You can't pick this card"
    queryUserCard f gs


queryYesNo :: String
           -> (GameState -> IO GameState)
           -> (GameState -> IO GameState)
           -> GameState -> IO GameState
queryYesNo msg fyes fno gs = do
  putStr msg
  response <- getLine
  case response of
    "yes" -> fyes gs
    "no"  -> fno gs
    _     -> queryYesNo msg fyes fno gs

queryChooseOne :: String
               -> [String]
               -> [(GameState -> IO GameState)]
               -> GameState -> IO GameState
queryChooseOne msg opts funs gs = do
  putStr msg
  response <- getLine
  case elemIndex response opts of
    Just i -> (funs !! i) gs
    Nothing -> queryChooseOne msg opts funs gs

queryGetString :: String -> [String] -> IO String
queryGetString msg opts = do
  putStrLn msg
  response <- getLine
  if elem response opts then return response else queryGetString msg opts  

chooseCard :: (GameState -> IO ())
            -> Validation
            -> (CardName -> GameState -> IO GameState)
            -> (CardName -> GameState -> IO GameState)
            -> GameState -> IO GameState
chooseCard display v eff back gs = do
  display gs
  card <- queryUserCard v gs
  if card == "back"
  then back card gs
  else eff card gs

foldrEffect :: (PlayerName -> GameState -> IO GameState) -> [PlayerName] -> GameState -> IO GameState
foldrEffect fun (p:ps) gs = do
  gs' <- fun p gs
  foldrEffect fun ps gs'
foldrEffect _ _ gs = return gs

doWhen :: Bool -> (GameState -> IO GameState) -> GameState -> IO GameState
doWhen b f gs = if b then f gs else return gs

cellarEffect :: GameState -> IO GameState
cellarEffect = discardGain 4 4

discardGain :: Int -> Int -> GameState -> IO GameState
discardGain 0 _ = return
discardGain start n = queryYesNo msg (fyes start n) (fno start n)
  where
    msg = "Would you like to discard anything? "
    fyes s x = chooseCard displayHand validations (doEffect s x) (\_ -> fno s x)
    doEffect s x c = discardGain s (x - 1) . updateCurrentPlayer (discardCard c)
    fno s x = return . updateCurrentPlayer (draw (s - x))
    validations = (allowBack . ensureConstraints) [ensureDiscard]

chapelEffect :: GameState -> IO GameState
chapelEffect = trashUpTo 4

chooseTrash :: GameState -> IO GameState
chooseTrash = chooseCard displayHand validations (\c -> return . trashCard c) (const return)
  where
    validations = (allowBack . ensureConstraints) [ensureTrash]

trashUpTo :: Int -> GameState -> IO GameState
trashUpTo 0 = return
trashUpTo n = queryYesNo msg fyes return
  where
    msg = "Would you like to trash anything? "
    fyes gs = do
      gs' <- chooseTrash gs
      trashUpTo (n - 1) gs'

moatEffect :: GameState -> PlayerName -> IO Bool
moatEffect gs pn = if hasCard "moat" (getPlayer gs pn)
                   then do
                     putStr msg
                     response <- getLine
                     case response of
                       "yes" -> do 
                          putStrLn $ pn ++ " revealed moat. "
                          return True
                       "no"  -> return False
                       _     -> moatEffect gs pn
                   else return False
  where
    msg = pn ++ ", would you like to reveal the moat card? "

chancellorEffect :: GameState -> IO GameState
chancellorEffect = queryYesNo msg fyes return
  where
    msg = "Would you like to discard your entire deck? "
    fyes = return . updateCurrentPlayer discardDeck

gainEffect :: Int -> Validation -> GameState -> IO GameState
gainEffect n vs = chooseCard (displayField filterField) vs doEffect (const return)
  where
    doEffect c gs = return $ updateCurrentPlayer (gainCard c) gs
    filterField _ cn = cost (getCard' cn) <= n

workshopEffect :: GameState -> IO GameState
workshopEffect gs = do
  putStrLn "Gain a card costing up to $4"
  gainEffect 4 (ensureGain 4) gs

bureaucratEffect :: GameState -> IO GameState
bureaucratEffect gs = 
  let ngs = updateCurrentPlayer (gainCard "silver") gs in
  foldrEffect revealVictory' (getOtherPlayersNames ngs) ngs
  where
    revealVictory' :: PlayerName -> GameState -> IO GameState
    revealVictory' pn gs' = do
      b <- moatEffect gs' pn
      if b then return gs' 
      else let v = filter (`hasType` Victory) (getPlayerHand gs' pn) in
        case v of
          [] -> do
                  putStrLn $ pn ++ " reveals " ++ show (getPlayerHand gs' pn) ++ " with no Victory cards."
                  return gs'
          [c] -> do
                   putStrLn $ pn ++ " reveals " ++ c ++ " and puts it on top of his/her deck."
                   return $ updatePlayer (putBackCard c) pn gs'
          _ -> do
                 putStrLn $ pn ++ ", choose a Victory card to put on top of your deck."
                 chooseCard (displayHand' pn) (validations pn) (doEffect pn) (\_ -> revealVictory' pn) gs'
    doEffect :: PlayerName -> CardName -> GameState -> IO GameState
    doEffect pn c gs' = do
      putStrLn $ pn ++ " reveals " ++ c ++ " and puts it on top of his/her deck."
      return $ updatePlayer (putBackCard c) pn gs'
    validations :: PlayerName -> Validation
    validations p = ensureConstraints [ensureDiscard' p, ensureIsVictory]

feastEffect :: GameState -> IO GameState
feastEffect gs = do
  putStrLn "Gain a card costing up to $5"
  gainEffect 5 (ensureGain 5) $ (addTrash ? rmFeast) "feast" gs
  where
    rmFeast cn = updateCurrentPlayer (rmFromDiscard cn)

militiaEffect :: GameState -> IO GameState
militiaEffect gs = foldrEffect moatMilitia (getOtherPlayersNames gs) gs
  where
    moatMilitia :: PlayerName -> GameState -> IO GameState
    moatMilitia pn gs' = do
      b <- moatEffect gs' pn
      if b then return gs'
      else discardTo3 pn gs'
    discardTo3 :: PlayerName -> GameState -> IO GameState
    discardTo3 pn gs' = do
      if length (getPlayerHand gs' pn) > 3 then
        do
          putStrLn $ pn ++ ", discard down to 3 cards."
          chooseCard (displayHand' pn) (validations pn) (doEffect pn) (\_ -> discardTo3 pn) gs'
      else
        return gs'
    doEffect :: PlayerName -> CardName -> GameState -> IO GameState
    doEffect pn c gs' = discardTo3 pn (updatePlayer (discardCard c) pn gs')
    validations :: PlayerName -> Validation
    validations p = ensureConstraints [ensureDiscard' p]

moneylenderEffect :: GameState -> IO GameState
moneylenderEffect gs = doWhen (ensureCardInHand gs "copper") doEffect gs
  where
    doEffect = return . trashCard "copper" . updateCurrentPlayer (addMoney 3)

remodelEffect :: GameState -> IO GameState
remodelEffect = chooseCard displayHand ensureTrash doEffect (const return)
  where
    cc c = getCardCost c + 2
    doEffect c gs = do
      putStrLn $ "Gain a card costing up to $" ++ show (cc c)
      gainEffect (cc c) (ensureGain (cc c)) (trashCard c gs)

spyEffect :: GameState -> IO GameState
spyEffect gs = fun (getPlayerNames gs) gs
  where
    fun :: [PlayerName] -> GameState -> IO GameState
    fun (pn:pns) gs' = do
      b <- moatEffect gs' pn
      if b then fun pns gs' else
        let gs0 = updatePlayer (draw 1) pn gs' in
        let r = ((getPlayerHand gs0 pn) \\ (getPlayerHand gs' pn)) in
        do
          putStrLn $ pn ++ " revealed " ++ (show r)
          s <- queryGetString ("(D)iscard " ++ (show r) ++ 
               " or (P)ut it back on " ++ pn ++ "'s deck: ") ["D","P"]
          case s of
            "D" -> fun pns (updatePlayer (discardCards r) pn gs0)
            "P" -> fun pns (updatePlayer (putBackCards r) pn gs0)
            _ -> undefined
    fun _ gs' = return gs'

thiefEffect :: GameState -> IO GameState
thiefEffect gs = do
  ngs <- fun (getOtherPlayersNames gs) gs
  let t = (trash ngs) \\ (trash gs)
  putStrLn $ show t
  chooseGain ngs t
  where
    fun :: [PlayerName] -> GameState -> IO GameState
    fun (pn:pns) gs' = do
      b <- moatEffect gs' pn
      if b then fun pns gs' else
        let p = getPlayer gs' pn in
        let gs0 = updatePlayer (draw 2) pn gs' in
        let r = (getPlayerHand gs0 pn) \\ (getPlayerHand gs' pn) in
        do
          putStrLn $ pn ++ " revealed " ++ (show r)
          gs1 <- fun2 gs0 pn (filter (`hasType` Treasure) r)
          let d =  (getPlayerHand gs1 pn) \\ (getPlayerHand gs' pn)
          let gs2 = updatePlayer (discardCards d) pn gs1
          fun pns gs2
    fun _ gs' = return gs'
    fun2 :: GameState -> PlayerName -> [CardName] -> IO GameState
    fun2 gs' pn [x] = do
      putStrLn $ "Trashing " ++ x
      return $ trashCard' x pn gs'
    fun2 gs' pn [x,y] = do
      s <- queryGetString ("Choose one to trash (" ++ x ++ " or " ++ y ++ "): ") [x,y]
      return $ trashCard' s pn gs'
    fun2 gs' _ _ = return gs'
    chooseGain :: GameState -> [CardName] -> IO GameState
    chooseGain gs' [] = return gs'
    chooseGain gs' (c:cs) = do
      putStrLn $ "Would you like to gain " ++ c ++ "?"
      response <- getLine
      case response of
        "yes" -> chooseGain (updateCurrentPlayer (gainCard c) gs') cs
        "no" -> chooseGain gs' cs
        _ -> chooseGain gs' (c:cs)

throneroomEffect :: GameState -> IO GameState
throneroomEffect gs = doWhen (any (`hasType` Action) (hand $ getCurrentPlayer gs)) go gs
  where
    go gs' = do
      putStrLn "Pick a card to play twice: "
      chooseCard displayHand ensurePlay doEffect (const return) gs'
    playTwice ce gs' = applyEffects ce gs' >>= applyEffects ce
    doEffect c gs' = playTwice (getCardEffect c) $ updateCurrentPlayer (discardCard c) gs'

councilroomEffect :: GameState -> GameState
councilroomEffect gs = updatePlayers (draw 1) (getOtherPlayersNames gs) gs

libraryEffect :: GameState -> IO GameState
libraryEffect = go []
  where
    go s gs = doWhen (length (hand (getCurrentPlayer gs)) < 7) (doEffect s) gs
    doEffect s gs = do
      let gs' = updateCurrentPlayer (draw 1) gs
      let fc = getFirstCard gs'
      gs'' <- doWhen (fc `hasType` Action) (queryYesNo (msg fc) (fyes fc s) (go s)) gs'
      go s gs''
    msg fc = "Do you want to set " ++ show fc ++ " aside? "
    fyes fc s gs = go (fc:s) (updateCurrentPlayer (rmFromHand fc) gs)

mineEffect :: GameState -> IO GameState
mineEffect = chooseCard displayHand v1 doEffect (const return)
  where
    v1 = ensureConstraints [ensureCardInHand, ensureIsTreasure]
    v2 cc = ensureConstraints [ensureIsTreasure, ensureCost cc]
    up3 cn = getCardCost cn + 3
    doEffect cn gs = do
      putStrLn $ "Gain a treasure card up to $" ++ show (up3 cn)
      chooseCard (displayField filteredField) (v2 (up3 cn)) doEffect2 (const return) gs
    doEffect2 cn = return . updateCurrentPlayer (addToHand cn)
    filteredField _ cn = cn `hasType` Treasure && getCardCost cn <= up3 cn

witchEffect :: GameState -> IO GameState
witchEffect gs = foldrEffect gainCurse (getOtherPlayersNames gs) gs
  where
    gainCurse :: PlayerName -> GameState -> IO GameState
    gainCurse pn gs' = do
      b <- moatEffect gs' pn
      if b then return gs'
      else do
        putStrLn $ pn ++ " gained a curse."
        return $ updatePlayer (gainCard "curse") pn gs'

revealUntil :: Int -> CardType -> GameState -> GameState
revealUntil 0 _ gs = gs
revealUntil n t gs = let gs' = updateCurrentPlayer (draw 1) gs in
                   let c = getFirstCard gs' in
                   if notTreasure c
                   then revealUntil n t (updateCurrentPlayer (discardCard c) gs')
                   else revealUntil (n - 1) t gs
  where
    notTreasure = not . (`hasType` t)

adventurerEffect :: GameState -> GameState
adventurerEffect = revealUntil 2 Action
