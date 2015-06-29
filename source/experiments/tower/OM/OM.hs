{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ConstraintKinds #-}

--
-- (c) Galois, Inc. 2014
-- Lee Pike
--
-- Oral Messages (without faults) in Tower.
--

--------------------------------------------------------------------------------

module Main where

import           Control.Monad hiding (when, forever)
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower
import           Tower.AADL

--------------------------------------------------------------------------------

-- Compose the system
system :: Tower () ()
system = do
  -- Make the general channel endpoints. The lieutenants share genRx.
  (genTx, genRx) <- channel
  -- Run the general every 1ms.
  genPer <- period (1`ms`)
  -- Make a message for the general to send ("42")
  monitor "general" (general (42::Sint32) genPer genTx)
  -- Make the lieutenants
  lieutenants genRx

-- The general: broadcast a single message.
general :: Constraints v
       => v
       -> Period
       -> ChanInput (Stored v)
       -> Monitor p ()
general v per tx = do
  handler per "general_broadcast" $ do
    e <- emitter tx 1
    -- Broadcast whatever message (v) is passed in.
    callback $ const $ emitV e v

-- Construct all the lieutenants.
lieutenants
  :: Constraints v
  => ChanOutput (Stored v)
  -> Tower () ()
lieutenants genRx = do
  -- Replicate three channels.
  chans <- replicateM 3 channel
  let (txs, rxs) = unzip chans
  -- Make each lieutenant.
  let mkLieutenant i =
        monitor ("lt" ++ show i) (lieutenant genRx tx rxs)
        where
          rxs = map snd chans
          tx  = fst (chans !! i)
  -- Construct each lieutenant.
  mapM_ mkLieutenant [0..2]

-- Compose the lieutenant from it's rounds.
lieutenant
  :: Constraints v
  => ChanOutput (Stored v)
  -- ^ General's channel
  -> ChanInput (Stored v)
  -- ^ Broadcast channel
  -> [ChanOutput (Stored v)]
  -- ^ All incoming channels from lieutenants
  -> Monitor p ()
lieutenant genTx tx rxs = do
  rnd1 genTx tx
  rnd2 rxs

-- Round 1 handler for a lieutenant: get the value from the general then
-- broadcast it to the other lieutenants.
rnd1 :: Constraints v
     => ChanOutput (Stored v)
     -> ChanInput (Stored v)
     -> Monitor p ()
rnd1 genTx tx =
  handler genTx "rnd1_handler" $ do
    e <- emitter tx 1
    callback $ \m -> do
      m' <- deref m
      -- Broadcast (emit) the message received from the General to all of the
      -- lieutenants.
      emitV e m'

-- Round 2 handler for a lieutenant.
rnd2 :: Constraints v
     => [ChanOutput (Stored v)]
     -> Monitor p ()
rnd2 rxs = do
  -- Local array to hold exchanged vals
  arr    <- state "local_arr"
  -- Counter for filling array
  bufCnt <- stateInit "bufCnt" (ival 0)
  -- Final result
  res    <- state "final_result"
  mapM_ (rnd2Handler arr bufCnt res (length rxs)) (zip rxs [0..])

-- Store incoming value from each lieutenant then perform a majority vote.
rnd2Handler
  :: Constraints v
  => Ref s (Array 3 (Stored v))
  -> Ref Global (Stored Sint32)
  -> Ref Global (Stored v)
  -> Int
  -> (ChanOutput (Stored v), Integer)
  -> Monitor p ()
rnd2Handler arr bufCnt res len (rx, i) =
  handler rx ("rnd2_handler_lieutenant_" ++ show i) $ do
    callback $ \msg -> do
      m  <- deref msg
      ix <- deref bufCnt
      -- Invariant: the index is within the array
      store (arr ! toIx ix) m
      -- Increment the index reference
      bufCnt += 1
      bc <- deref bufCnt
      -- When the array is full, vote the result; reset the index.
      when (bc >=? fromIntegral len)
           (do store bufCnt 0
               -- Vote on the result.
               v <- iVote arr
               -- Store the result in local memory.
               store res v
           )

--------------------------------------------------------------------------------

-- Fast majory vote over Ivory arrays.
iVote :: ( ANat n
         , GetAlloc eff ~ Scope s'
         , Constraints v
         ) => Ref s (Array n (Stored v))
           -> Ivory eff v
iVote arr = do
  cR   <- local izero
  cntR <- local (izero :: Init (Stored Uint32))
  arrayMap $ \ix -> do
    x   <- deref (arr ! ix)
    c   <- deref cR
    cnt <- deref cntR
    cond_
      [ (x ==? c)
      ==> (cntR += 1)
      , cnt ==? 0
      ==> do store cR x
             store cntR 1
      , true
      ==> cntR += (-1)
      ]
  return =<< deref cR

--------------------------------------------------------------------------------
-- Helpers

-- Fast majority votes over lists.
vote :: Eq a => [a] -> a
vote []     = error $ "Not enough elems!"
vote (l:ls) = vote' l 1 ls
  where
  vote' :: Eq a => a -> Int -> [a] -> a
  vote' c cnt [] = c
  vote' c cnt (x:rst)
    | c == x
    = vote' c (cnt+1) rst
    | cnt == 0
    = vote' x 1 rst
    | otherwise
    = vote' c (cnt-1) rst

-- The type of a period channel
type Period = ChanOutput (Stored ITime)

-- A bunch of type constraints.
type Constraints v =
  ( IvoryEq v
  , IvoryZeroVal v
  , IvoryType v
  , IvoryInit v
  , IvoryStore v
  )

--------------------------------------------------------------------------------

-- -- Generate the AADL
-- main :: IO ()
-- main = runCompileAADL opts system
--   where
--   opts = initialOpts { genDirOpts = Just "AADL" }
