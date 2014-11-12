{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE QuasiQuotes #-}

-- (c) Galois, Inc. 2014
-- Lee Pike
--
-- Example distributed system in Tower syntax. Three synchronous nodes, each
-- sending its state (an Int) to it's right neighbor, then incrementing its
-- state by 1.
--
--  -----------------------------------
--  |                                 |
--  |-- > node0 --> node1 --> node2 --|
--

--------------------------------------------------------------------------------

module DistSys where

import           Control.Monad
import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
--import qualified Ivory.OS.FreeRTOS.SearchDir as FreeRTOS
import           Ivory.Tower

--------------------------------------------------------------------------------

data TestPlatform

type Period = ChanOutput (Stored ITime)
type State = Stored Uint32

system :: Tower TestPlatform ()
system = do
  -- Make the three channels.
  (in0, out0) <- channel
  (in1, out1) <- channel
  (in2, out2) <- channel
  -- Run sychronously at 1ms.
  per <- period (Microseconds 1000)
  -- Creat the 3 nodes, passing them the global clock and the channel
  -- input/ouputs.

--  node "nod0" per out2 in0
  node "nod1" per out0 in1
  node "nod2" per out1 in2

  obs out0 out1


obs ::  ChanOutput State
     -> ChanOutput State
     -> Tower p ()
obs out0 out1 =
  monitor "obs" go
  where
  go = do
    -- Initialize local state
    st0 <- state "obs_st0"
    st1 <- state "obs_st1"
    handler out0 "obs_handler0" $
      callback $ \m -> store st0 =<< deref m
    handler out1 "obs_handler1" $
      callback $ \m -> store st1 =<< deref m

-- observer s0 s1 s3 = monitor "observer" $ do
--   handler s0 s1 s2 "observer_prop" $

-- A node in the system.
node :: String
     -> Period
     -> ChanOutput State
     -> ChanInput State
     -> Tower p ()
node name per rx tx =
  monitor name go
  where
  go = do
    -- Initialize local state
    st <- state (name ++ "_st")
    updateState name st rx
    sendMsg name per st tx

-- How to update local state: Tak an incoming message, get the value, and
-- increment by one.
updateState :: String
            -> Ref s State
            -> ChanOutput State
            -> Monitor p ()
updateState name st rx =
  handler rx (name ++ "_recv_msg") $
    callback $ \m -> call_ update m st

[ivory|
void update(const * State m, * State st) {
   store st as *m+1;
}
|]

sendMsg :: String
        -> ChanOutput (Stored ITime)
        -> Ref s State
        -> ChanInput State
        -> Monitor p ()
sendMsg name per st tx =
  handler per (name ++ "_tick") $ do
    e <- emitter tx 1
    callback $ \_ -> emit e (constRef st)


main :: IO ()
main = do
  void (runCompilerWith Nothing searchpath code compileropts)
  where
  dir = "distsystest"
  (_ast, code) = tower system
  searchpath = Just [FreeRTOS.searchDir]
  compileropts = initialOpts { srcDir = dir, includeDir = dir }
