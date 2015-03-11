{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ConstraintKinds #-}

--
-- (c) Galois, Inc. 2015
-- Lee Pike
-- Benjamin Jones
--
-- Oral Messages (without faults) in Tower, composed with a Synchronous
-- Observer.
--

--------------------------------------------------------------------------------

module Main where

import           Control.Monad hiding (when)
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower
import           Tower.AADL

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- lieutenant start

lieutenants
  :: Constraints v
  => ChanOutput (Stored v)    -- ^ channel from the General
  -> Int                      -- ^ 'n', no. of lieutenants
  -> [Ref Global (Stored v)]  -- ^ storage for vote results (length 'n')
  -> Tower () ()
lieutenants genRx n refs = do
  -- Replicate n channels.
  chans <- replicateM n channel
  let (txs, rxs) = unzip chans
  -- Make each lieutenant.
  let mkLieutenant i =
        monitor ("lt" ++ show i) (lieutenant genRx tx rxs (refs !! i))
        where
          rxs = map snd chans
          tx  = fst (chans !! i)
  -- Construct each lieutenant.
  mapM_ mkLieutenant [0..n-1]

-- Compose the lieutenant from it's rounds.
lieutenant
  :: Constraints v
  => ChanOutput (Stored v)
  -- ^ General's channel
  -> ChanInput (Stored v)
  -- ^ Broadcast channel
  -> [ChanOutput (Stored v)]
  -- ^ All incoming channels from lieutenants
  -> Ref Global (Stored v)
  -- ^ place to store vote result
  -> Monitor p ()
lieutenant genTx tx rxs ref = do
  rnd1 genTx tx
  rnd2 rxs ref

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

-- Round 2 handler for a lieutenant: store incoming values and perform the
-- majority vote, storing it at the global reference 'res'
rnd2 :: Constraints v
     => [ChanOutput (Stored v)]  -- ^ channels for incoming messages
     -> Ref Global (Stored v)    -- ^ place to store voting result
     -> Monitor p ()
rnd2 rxs res = do
  -- Local array to hold exchanged vals
  arr    <- state "local_arr"
  -- Counter for filling array
  bufCnt <- state "bufCnt"
  mapM_ (rnd2Handler arr bufCnt res (length rxs)) (zip rxs [0..])

-- Store incoming value for each lieutenant, do majority vote.
rnd2Handler
  :: Constraints v
  => Ref s (Array 3 (Stored v))
  -> Ref Global (Stored Sint32)
  -> Ref Global (Stored v)
  -> Int
  -> (ChanOutput (Stored v), Integer)
  -> Monitor p ()
rnd2Handler arr bufCnt res len (rx, i) =
  handler rx ("rnd2_handler_lieutenant_" ++ show i) $
    callback $ \m -> do
      bc <- deref bufCnt
      -- If the buffer is full (buffer count as big as the array), then
      ifte_ (bc >=? fromIntegral len)
        (do -- Vote on the result.
            v <- iVote arr
            -- Store the result in local memory.
            store res v
        )
        (do m' <- deref m
            -- Store the incoming value into the local array.
            store (arr ! fromInteger i) m'
            -- Increment buffer count.
            bufCnt += 1
        )

-- lieutenant end
--------------------------------------------------------------------------------

-- Compose the system
system :: Tower () ()
system = do
  -- Make the general channel endpoints
  (genTx, genRx) <- channel
  -- Run the general every 1ms.
  genPer <- period (1`ms`)
  -- global program counter
  pc <- state "program_counter"
  -- Make a message for the general to send ("42")
  monitor "general" (general pc (42::Sint32) genPer genTx)
  -- Make the lieutenants
  refs <- mapM (\i -> state ("vote_" ++ show i)) [0..numLieuts-1]
  lieutenants pc genRx numLieuts refs
  -- Make the observer
  observer pc genPer refs ok

-- | TODO: finish observer
observer :: Constraints v
         => Ref Global (Stored Sint32)  -- ^ program counter
         -> Period
         -> [Ref Global (Stored v)]   -- ^ the votes to check
         -> Ref Global (Stored Bool)  -- ^ 'ok' flag indicates validity and agreement
                                      -- as long as @@'aok' == True@@
         -> Monitor p ()
observer per pc refs ok = do
  -- update on clock?
  -- update after recv'ing votes?
  let valid c rs = all (==? c) rs  -- not quite correct
      agree [] = True
      agree (v:rest) = valid v rest
  handler per "observe_props" $
    callback $ \_ -> do
      pc' <- deref pc
      cond_
        [ (pc' ==? fromIntegeral 4 &&
              (not (valid correctVal refs) ||
               not (agree refs)))
          ==> store ok False,
          True
          ==> return ()
        ]

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

-- Generate the AADL
main :: IO ()
main = runCompileAADL opts system
  where
  opts = initialOpts { genDirOpts = Just "AADL" }

--------------------------------------------------------------------------------
-- Testing

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
