module AtomOM1
  ( compileOM1 )
where

import Control.Monad (forM_)
import Data.Word

import Language.Atom


-- Parameters ----------------------------------------------------------


-- Node clock periods (in ticks)
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

newChannel :: String -> Atom (ChanInput, ChanOutput)
newChannel = flip channel missingMsgValue

-- | Top level rule
om1 :: Atom ()
om1 = do
  -- setup channels for communication between source, relays, and receivers
  s2rs <- mapM newChannel [ tg "s2r" i | i <- relaySet ]
  r2rs <- mapM (mapM newChannel) [ [ tg2 "r2r" i j | j <- recvSet ]
                                                   | i <- relaySet ]
  votes <- mapM msgVar [ tg "vote" j | j <- recvSet ]


  -- declare source node
  source (map fst s2rs)

  -- declare relay nodes
  forM_ relaySet $ \ident ->
    relay ident (snd (s2rs !! ident))
                (map fst (r2rs !! ident))

  -- declare receiver nodes
  forM_ recvSet $ \ident ->
    recv ident [ snd ((r2rs !! i) !! ident) | i <- relaySet ]
         (votes !! ident)

  -- declare the observer
  observer


-- Source --------------------------------------------------------------

-- | Source node ("General")
source :: [ChanInput]  -- ^ output channels
       -> Atom ()
source cs = period sourcePeriod
          . atom "source" $ do
  done <- bool "done" False
  cond $ not_ (value done)

  forM_ cs $ \c -> do
    writeChannel c goodMsg

  done <== Const True


-- Relays --------------------------------------------------------------

-- | Relay node ("Lieutenant")
relay :: Int          -- ^ relay id
      -> ChanOutput   -- ^ channel from source
      -> [ChanInput]  -- ^ channels to receivers
      -> Atom ()
relay ident inC outCs = period relayPeriod
                      . atom (tg "relay"  ident) $ do
  done <- bool "done" False
  msg  <- msgVar (tg "relay_msg" ident)
  cond $ isMissing msg  -- we haven't stored a value yet
  condChannel inC       -- there is a value available

  msg  <== readChannel inC
  forM_ outCs $ \c -> do
    let m = readChannel inC :: E MsgType
    writeChannel c m
  done <== Const True


-- Receivers -----------------------------------------------------------

-- | Receiver node ("Lieutenant")
recv :: Int  -- ^ receiver id
     -> [ChanOutput]  -- ^ channels from relays
     -> V MsgType
     -> Atom ()
recv ident inCs vote = period recvPeriod
                . atom (tg "recv" ident) $ do
  done <- bool "done" False
  buffer <- mapM msgVar [ tg (tg "buffer" ident) i | i <- relaySet ]

  forM_ relaySet $ \i -> do
    atom (tg2 "recv_poll" ident i) $ do
      cond $ isMissing (buffer !! i)
      condChannel (inCs !! i)
      (buffer !! i) <== readChannel (inCs !! i)

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
        -- rules:
        -- x ==. y   = (y, c+1)
        -- c == 0    = (x, 1)
        -- otherwise = (y, c-1)
        onTrue1       = y
        onTrue2       = c + (Const 1)
        onFalse1      = mux (c ==. Const 0) x y
        onFalse2      = mux (c ==. Const 0) (Const 1) (c - (Const 1))
        _             = c :: E Word64

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

-- | Specially designated intended message to be send in the absense of faults
goodMsg :: E MsgType
goodMsg = Const 0

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
