{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad

import Ivory.Tower
import Ivory.Language
import Tower.AADL

----------------------------------------

type Signal = Bool

maxCnt = 3

data Signal = Signal Bool



----------------------------------------


pushButton :: Tower e ()
pushButton = do

  -- Buttons
  lo   <- signal
  med  <- signal
  max  <- signal

  perClkA <- period (20`ms`)
  perClkB <- period (20`ms`)

  -- From COM to MON
  (comMonATx, comMonARx) <- channel
  -- From MON to COM
  (monComATx, monComARx) <- channel

  -- From A to B
  (comATx, comBRx) <- channel
  -- From B to A
  (comBTx, comARx) <- channel

----------------------------------------

  monitor "A_COM" $ do
    st <- state "selected_mode" :: Signal
    active <- stateInit "active" True

    -- Handle the button signals
    handleButton st lo med max

    -- Handle my monitor's yield request
    handler monComARx "fromMon" $ do
      e <- emitter comATx 1
      callback $ \msg ->
        store active False
        -- Send a yield request to B
        when (not msg) (emit e msg)

    -- Provide monitor my selected mode
    handler perClkA "clk" $ do
      e <- emitter comMonATx 1
      callback $ \_ -> emit e st


----------------------------------------

  monitor "A_MON" $ do
    lm  <- state "local_mode"
    cm  <- state "com_mode"
    cnt <- stateInit "disagree_cnt" 0

    -- Handle the button signals
    handleButton lm lo med max

    -- Store COM's mode
    handler comMonARx "monRx" $ do
      callback $ \msg -> store cm msg

    -- Handle periodic exchange
    handler perClkB "clk" $ do
      e <- emitter monComATx 1
      callback $ \_ -> do
        if lm == cm
          then store cnt 0
          else store cnt (cnt+1)
        if cnt >= maxCnt
          -- Report disagreement
--          then emit e Yield
          then emit e undefined
          else undefined


----------------------------------------

handleButton st lo med max = do
  -- Handle the button signals
  -- XXX collapse into one handler?
  handler lo   "low" $ do
    callback $ \msg -> store st msg
  handler med  "med" $ do
    callback $ \msg -> store st msg
  handler max "max" $ do
    callback $ \msg -> store st msg

