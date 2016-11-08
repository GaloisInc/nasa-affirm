module AtomSMP
  ( smp
  )
where

import Control.Monad (forM_)
import Data.Int
import System.IO

import Language.Atom hiding (compile)
import qualified Language.Atom as A
import Language.Sally


-- Parameters ----------------------------------------------------------

-- Time in ticks between each node's activity
initPeriod     = 5
sourcePeriod   = 5
recvPeriod     = 5


-- Messages ------------------------------------------------------------

type MsgType = Int64
msgType = Int64

-- | Special message value indicating "no message present"
missingMsgValue :: MsgType
missingMsgValue = 0

-- | Special message value indicating "correct (intended) message"
goodMsg :: E MsgType
goodMsg = Const 1

-- | Another good message
goodMsg' :: E MsgType
goodMsg' = Const 2

-- SMP Spec ------------------------------------------------------------

-- | Top level rule
smp :: Atom ()
smp = do
  -- setup channel for communication between source and receiver
  (s2rIn, s2rOut)   <- channel "s2r_1" msgType
  (s2rIn', s2rOut') <- channel "s2r_2" msgType

  -- declare system nodes:

  -- used to print out probe values only
  sysInit

  -- source:
  source s2rIn s2rIn'
  -- receiver:
  recv "recv1" s2rOut
  recv "recv2" s2rOut'


-- Init ----------------------------------------------------------------

-- | Print all probe values at phase 0
sysInit :: Atom ()
sysInit = period initPeriod . exactPhase 0 . atom "sysInit" $ do
  printAllProbes


-- Source --------------------------------------------------------------

-- | Source node
source :: ChanInput  -- ^ channel input source will put messages on for recv 1
       -> ChanInput  -- ^ channel input source will put messages on for recv 1
       -> Atom ()
source c c' = period sourcePeriod . atom "source" $ do
  done <- bool "done" False
  probe "source.done" (value done)

  cond $ not_ (value done)
  writeChannel c  goodMsg  -- send 'goodMsg' to recv 1
  writeChannel c' goodMsg' -- ditto for recv 2
  done <== Const True

  printAllProbes

-- Receiver -----------------------------------------------------------

-- | Receiver node
recv :: String      -- ^ name for the receiver
     -> ChanOutput  -- ^ channel output that receiver listens to
     -> Atom ()
recv nm c = period recvPeriod . atom nm $ do
  done <- bool "done" False
  probe (nm ++ "_done") (value done)
  vote <- int64 (nm ++ "_vote") missingMsgValue
  probe (nm ++ "_vote") (value vote)

  cond $ fullChannel c    -- execute only if channel 'c' has new message
  vote <== readChannel c  -- read message and assign to 'vote'
  done <== Const True

  printAllProbes

printAllProbes :: Atom ()
printAllProbes = mapM_ printProbe =<< probes
