{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ConstraintKinds #-}

-- (c) Galois, Inc. 2014
-- Lee Pike
--
-- Oral Messages (without faults) in Tower.
--

--------------------------------------------------------------------------------

module OM where

import           Control.Monad hiding (when)
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower
import           Tower.AADL

--------------------------------------------------------------------------------

type Period = ChanOutput (Stored ITime)
type State = Stored Uint32

type Constraints v =
  ( IvoryEq v
  , IvoryZeroVal v
  , IvoryType v
  , IvoryInit v
  , IvoryStore v
  )

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

general :: Constraints v
       => v
       -> Period
       -> ChanInput (Stored v)
       -> Monitor p ()
general v per tx = do
  handler per "broadcast" $ do
    e <- emitter tx 1
    callback $ const $ emitV e v

rnd1 ::  Constraints v
     => ChanOutput (Stored v)
     -> ChanInput (Stored v)
     -> Ref Global (Stored Uint32)
     -> Monitor p ()
rnd1 rx tx rnd =
  handler rx "rnd1" $ do
    e <- emitter tx 1
    callback $ \m -> do m' <- deref m
                        emitV e m'
                        rnd += 1

rnd2 :: Constraints v
     => ChanOutput (Stored v)
     -> ChanOutput (Stored v)
     -> ChanOutput (Stored v)
     -> Ref Global (Stored Uint32)
     -> Monitor p ()
rnd2 rx0 rx1 rx2 rnd = do
  arr <- state "local_arr"
  mapM_ (storeMsg arr rnd) (zip [rx0, rx1, rx2] [0..])

storeMsg
  :: Constraints v
  => Ref s (Array 3 (Stored v))
  -> Ref Global (Stored Uint32)
  -> (ChanOutput (Stored v), Integer)
  -> Monitor p ()
storeMsg arr rnd (rx, i) = do
  res <- state "result"
  handler rx ("rnd" ++ show i) $
    callback $ \m -> do
      r <- deref rnd
      when (r ==? 1) $ do
        m' <- deref m
        store (arr ! fromInteger i) m'
        v <- iVote arr
        store res v
lieutenant
  :: Constraints v
  => ChanOutput (Stored v)
  -> ChanInput (Stored v)
  -> [ChanOutput (Stored v)]
  -> Monitor p ()
lieutenant gOut tx [r0Tx, r1Tx, r2Tx] = do
  rnd <- stateInit "rnd" izero
  rnd1 gOut tx rnd
  rnd2 r0Tx r1Tx r2Tx rnd

system :: Tower () ()
system = do
  (gIn, gOut) <- channel
  ltChans <- replicateM 3 channel
  -- Run sychronously at 1ms.
  per <- period (1`ms`)
  monitor "general" (general (42::Sint32) per gIn)

  let mkLieutenant i = monitor ("lt" ++ show i) $
        lieutenant gOut (fst (ltChans !! i)) (map snd ltChans)
  mapM_ mkLieutenant [0..2]

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
