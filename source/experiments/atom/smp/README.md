# atom-smp

A specification in [Atom](https://github.com/tomahawkins/atom) (Haskell eDSL) for a
concurrent simple message passing system. The system has two nodes. One node,
the "source", sends a single message to another node, the "receiver".

This repository contains the specification for 'smp' in `AtomSMP.hs` and the
code for translating the spec to C and Sally in `Main.hs`.


# Usage

Compilation requires GHC, cabal, and a C99 compatible C compiler, and copies
of the GaloisInc `atom` and `atom-sally` repositories in a common directory.
Once the dependencies are met you can install and run the translators using
the `install.sh` script like so:

```
$ mkdir deps
$ cd deps
$ git clone https://github.com/GaloisInc/atom
$ git clone https://github.com/GaloisInc/atom-sally
$ cd ..

$ ATOMDIR=deps ./install.sh
```

This will build the C and Sally translators and translate the 'smp'
specification to code `smp.{c,h}` and model `smp.mcmt`, respectively.


# Simplified View of the specification

Here is a simplified view of the spec, without the probes and compiler
details. It consists of a source node which sends a message to two receivers.
Each receiver stores the incoming message in a local variable.

```haskell
-- | Special message value indicating "no message present"
missingMsgValue = 0

-- | Special message value indicating "correct (intended) message"
goodMsg = Const 1

-- | Another good message
goodMsg' = Const 2

-- Top level SMP Spec ---------------------------------------------------

smp :: Atom ()
smp = do
  -- setup channel for communication between source and receiver
  (s2rIn, s2rOut)   <- channel "s2r_1" missingMsgValue
  (s2rIn', s2rOut') <- channel "s2r_2" missingMsgValue

  -- declare source node
  source s2rIn s2rIn'

  -- declare 2 receiver nodes
  recv "recv1" s2rOut
  recv "recv2" s2rOut'

-- Source Node ---------------------------------------------------------

source :: ChanInput  -- ^ channel input source will put messages on for recv 1
       -> ChanInput  -- ^ channel input source will put messages on for recv 1
       -> Atom ()
source c c' = period sourcePeriod . atom "source" $ do
  -- declare local variables
  done <- bool "done" False
  -- activation conditions
  cond $ not_ (value done)
  writeChannel c  goodMsg  -- send 'goodMsg' to recv 1
  writeChannel c' goodMsg' -- ditto for recv 2
  -- behavior
  done <== Const True

-- Receiver Node ------------------------------------------------------

recv :: String      -- ^ name for the receiver
     -> ChanOutput  -- ^ channel output that receiver listens to
     -> Atom ()
recv nm c = period recvPeriod . atom nm $ do
  -- declare local variables
  done <- bool "done" False
  vote <- int64 (nm ++ "_vote") missingMsgValue
  -- activation conditions
  condChannel c           -- execute only if channel 'c' has new message
  -- behavior
  vote <== readChannel c  -- read message and assign to 'vote'
  done <== Const True
```
