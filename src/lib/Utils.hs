module Lib.Utils where

import System.Random
import Data.Array.IO
import Data.Array.ST
import Data.STRef

import Control.Monad
import Control.Monad.ST


(?) :: (t -> a -> b) -> (t -> b -> c) -> t -> a -> c
(?) f g t = g t . f t

shuffle :: [a] -> Int -> [a]
shuffle xs seed = runST $ do
  g <- newSTRef gen
  let randomRST lohi = do
      (a,s') <- liftM (randomR lohi) (readSTRef g)
      writeSTRef g s'
      return a
  ar <- newArr n xs
  forM [1..n] $ \i -> do
    j <- randomRST (i,n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArr :: Int -> [a] -> ST s (STArray s Int a)
    newArr m =  newListArray (1,m)
    gen = mkStdGen seed
