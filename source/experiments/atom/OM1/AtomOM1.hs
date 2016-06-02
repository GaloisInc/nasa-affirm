module AtomOM1
  ( compileOM1 )
where

import Control.Monad (forM_)
import Data.Word

import Language.Atom


-- Parameters ----------------------------------------------------------

-- Time in ticks between each node's activity
initPeriod     = 100
sourcePeriod   = 20
relayPeriod    = 7
recvPeriod     = 13
observerPeriod = 1

numRelays  = 3
numRecvs   = 3

relaySet = [0..numRelays-1]
recvSet  = [0..numRecvs-1]


-- OM(1) Spec ------------------------------------------------------------

-- | Top level rule
om1 :: Atom ()
om1 = do
  -- setup channels for communication between source, relays, and receivers
  s2rs <- mapM msgVar [ tg "s2r" i | i <- relaySet ]
  r2rs <- mapM (mapM msgVar) [ [ tg2 "r2r" i j | j <- recvSet ]
                                               | i <- relaySet ]
  votes <- mapM msgVar [ tg "vote" j | j <- recvSet ]

  -- print initial probe values
  period initPeriod . atom "init" $ do
    done <- bool "done" False
    atom "init_poll" $ do
      cond $ not_ (value done)
      printStrLn "Initial probe values:"
      ps <- probes
      mapM_ printProbe ps
      done <== Const True

  -- declare system nodes
  source s2rs

  forM_ relaySet $ \ident ->
    relay ident (s2rs !! ident) (r2rs !! ident)

  forM_ recvSet $ \ident ->
    recv ident [ (r2rs !! i) !! ident | i <- relaySet ] (votes !! ident)

  -- declare the observer
  observer


-- Source --------------------------------------------------------------

-- | Source node ("General")
source :: [V MsgType]  -- ^ output channels
       -> Atom ()
source cs = period sourcePeriod
          . atom "source" $ do
  done <- bool "done" False
  let source_msg = Const 1 :: E Word64

  atom "source_poll" $ do
    cond $ not_ (value done)
    forM_ cs $ \c -> do
      c <== source_msg
    done <== Const True


-- Relays --------------------------------------------------------------

-- | Relay node ("Lieutenant")
relay :: Int          -- ^ relay id
      -> V MsgType    -- ^ input channel
      -> [V MsgType]  -- ^ output channel(s)
      -> Atom ()
relay ident inC outCs = period relayPeriod
                      . atom (tg "relay"  ident) $ do
  done <- bool "done" False
  msg  <- msgVar (tg "relay_msg" ident)

  atom (tg "relay_poll" ident) $ do
    cond $ isMissing msg         -- we haven't stored a value yet
    cond $ not_ (isMissing inC)  -- there is a value available
    msg  <== value inC
    forM_ outCs $ \c -> do
      c <== value inC
    done <== Const True


-- Receivers -----------------------------------------------------------

-- | Receiver node ("Lieutenant")
recv :: Int  -- ^ receiver id
     -> [V MsgType]  -- ^ input channels
     -> V MsgType    -- ^ vote
     -> Atom ()
recv ident inCs vote = period recvPeriod
                . atom (tg "recv" ident) $ do
  done <- bool "done" False
  buffer <- mapM msgVar [ tg (tg "buffer" ident) i | i <- relaySet ]

  forM_ relaySet $ \i -> do
    atom (tg2 "recv_poll" ident i) $ do
      cond $ not_ (isMissing (inCs !! i))
      cond $ isMissing (buffer !! i)
      (buffer !! i) <== value (inCs !! i)

  atom (tg "recv_vote" ident) $ do
    cond $ all_ (not_ . isMissing) buffer
    vote <== computeVote (value <$> buffer)
    done <== Const True


-- | Boyer-Moore Fast Majority Vote
computeVote :: [E MsgType] -> E MsgType
computeVote = fst . foldr iter (missingMsgValueE, Const 0)
  where
    iter x (y, c) = ( mux (x ==. y) onTrue1 onFalse1
                    , mux (x ==. y) onTrue2 onFalse2)
      where
        onTrue1       = y
        onTrue2       = c + (Const 1)
        onFalse1      = mux (c ==. Const 0) x y
        onFalse2      = mux (c ==. Const 0) (Const 1) (c - (Const 1))
        _             = c :: E Word64
     -- rules:
     -- x ==. y   = (y, c+1)
     -- c == 0    = (x, 1)
     -- otherwise = (y, c-1)

-- | Synchronous observer node; current prints probe values to console at
-- phase 0.
observer :: Atom ()
observer = period observerPeriod
         . exactPhase 0
         . atom "observer" $ do
  ps <- probes
  mapM_ printProbe ps


-- Messages ------------------------------------------------------------

type MsgType = Word64

-- | Special message type value indicating "no message present"
missingMsgValue :: MsgType
missingMsgValue = 0

missingMsgValueE :: E MsgType
missingMsgValueE = Const 0

isMissing :: V MsgType -> E Bool
isMissing = (==. missingMsgValueE) . value

-- | Declare a variable of message type and add a probe for it to the
-- environment
msgVar :: Name -> Atom (V MsgType)
msgVar nm = do
  v <- msgVar' nm
  probe nm (value v)
  return v

-- | Declare a message variable w/o adding a probe
msgVar' :: Name -> Atom (V MsgType)
msgVar' nm = word64 nm missingMsgValue

-- | Tag a name with an ID
tg :: Name -> Int -> Name
tg nm i = nm ++ "_" ++ show i

tg2 :: Name -> Int -> Int -> Name
tg2 nm i j = nm ++ "_" ++ (show i) ++ "_" ++ (show j)


-- Variable Channels ---------------------------------------------------

type VChannel a = Channel (V a)

vchannel :: V a -> Atom (VChannel a)
vchannel = channel

-- | Write the value of the held variable to the channel; sets 'hasData' to
-- True
writeVChannel :: VChannel a -> Atom ()
writeVChannel = writeChannel

-- | Read the channel if it has data available; sets 'hasData' to False
readVChannel :: VChannel a -> Atom (E a)
readVChannel c = value <$> readChannel c

-- | Read the channel without affecting its 'hasData' state
obsVChannel :: VChannel a -> Atom (E a)
obsVChannel (Channel var _) = return $ value var

-- | Update the value of the variable held by the channel
updateVChannel :: Assign a => VChannel a -> E a -> Atom ()
updateVChannel (Channel var _) expr = do
  var <== expr


-- Code Generator ------------------------------------------------------

-- | Invoke the atom compiler, generating 'om1.{c,h}'
-- Also print out info on the generated schedule.
compileOM1 :: IO ()
compileOM1 = do
  (sched, _, _, _, _) <- compile "om1" cfg om1
  putStrLn $ reportSchedule sched
  where
    cfg = defaults { cCode = prePostCode }

-- | Custom pre-post code for generated C
prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode _ _ _ =
  ( unlines [ "#include <stdio.h>"
            , "#include <unistd.h>"
            , ""
            , "// ---- BEGIN of source automatically generated by Atom ----"
            ]
  , unlines [ "// ---- END of source automatically generated by Atom ----"
            , ""
            , "int main(int argc, char **argv) {"
            , "  while(1) { om1(); usleep(500); }"
            , "}"
            ]
  )
