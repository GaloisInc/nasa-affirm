% November AFFIRM Progress
% Lee Pike

# This Past Month

- Chat with Bruno
- OM(1) in ADSL
- AADL generation

# The Setup

- No faults yet
- Fixed number of nodes

```
Rnd0:
-----
G --> L_0
  --> L_1
  --> L_2

Rnd1:
-----
L_i --> L_k (0 <= i,k <= 2)

Rnd2:
-----
Vote
```

# ADSL Spec

- 50LOCs (no comments, types)
- Biggest function: majority voting

# Make the system
```haskell
system :: Tower () ()
system = do
  -- Make the general channel endpoints
  (genTx, genRx) <- channel
  -- Run the general every 1ms.
  genPer <- period (1`ms`)
  -- Make a message for the general to send ("42")
  monitor "general" (general (42::Sint32) genPer genTx)
  -- Make the lieutenants
  lieutenants genRx
```

# The General

```haskell
-- The general: broadcast a single message.
general v per tx = do
  handler per "general_broadcast" $ do
    e <- emitter tx 1
    -- Broadcast whatever message (v) is passed in.
    callback $ const $ emitV e v
```

# The Lieutenants (Rnd 1)

```haskell
lieutenant genTx tx rxs = do
  rnd1 genTx tx
  rnd2 rxs

-- Round 1 handler for a lieutenant: get the value from
-- the general then broadcast it to the other lieutenants.
rnd1 genTx tx =
  handler genTx "rnd1_handler" $ do
    e <- emitter tx 1
    callback $ \m -> do
      m' <- deref m
      -- Broadcast (emit) the message received from the
      -- General to all of the lieutenants.
      emitV e m'
```

# The Lieutenants (Rnd 2)

```haskell
-- Round 2 handler for a lieutenant.
rnd2 rxs = do
  . . .

  -- If the buffer is full
  -- (buffer count as big as the array)
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
```

# AADL Generation

- Extracts from the model component connections
- Omits behavior (e.g., vote)

# Process definition

```
process implementation system_process.impl
  subcomponents
    th0: thread thread_period_1ms;
    th1: thread lt2_34;
    th2: thread lt1_21;
    th3: thread lt0_8;
    th4: thread general_1;
  connections
    th0_to_th4: port th0.Output1ms -> th4.Input1ms;
    th1_to_th1: port th1.Outputsync_7 -> th1.Inputsync_7;
    th1_to_th2: port th1.Outputsync_7 -> th2.Inputsync_7;
    . . .
end system_process.impl;
```

# General Specification (1/2)


\small

```
thread general_1
  -- No source location
  features
    Input1ms: in event data port Base_Types::Integer_64
      {
        SYS::Compute_Entrypoint_Source_Text => ("callback_4");
        Source_Text => ("general_1.c");
      };
    Outputsync_0: out event data port Base_Types::Integer_32
      {
        SYS::CommPrim_Source_Text => ("emitter_3_chan_0");
      };
```

# General Specification (2/2)

```
  properties
    SYS::Sends_Events_To => "{{1 Output0}}";
    SYS::Thread_Type => Passive;
    Dispatch_Protocol => Aperiodic;
    Compute_Execution_Time => 10 us .. 100 us;
    Stack_Size => 100 bytes;
    Priority => 1;
end general_1;
```

# Current Issues

- Modes
    - Handlers: triggered by input
    - Input is clock, event, or "call"
    - Conditions (modes) would be useful
    - E.g., specify rounds

- Behavior/architecture
    - In our current design, behavior intermingled with arch.

- AADL
    - Currently in a "multi-threaded" style
    - Would like to abstract (not sure if possible)

# Next Month

- Add hybrid faults
- Explore verification




<!-- ```Haskell -->
<!-- system :: Tower TestPlatform () -->
<!-- system = do -->
<!--   -- Make the three channels. -->
<!--   (in0, out0) <- channel -->
<!--   (in1, out1) <- channel -->
<!--   (in2, out2) <- channel -->
<!--   -- Run sychronously at 1ms. -->
<!--   per <- period (Microseconds 1000) -->
<!--   -- Creat the 3 nodes, passing them the global clock and the channel -->
<!--   -- input/ouputs. -->
<!--   node "nod0" per out2 in0 -->
<!--   node "nod1" per out0 in1 -->
<!--   node "nod2" per out1 in2 -->
<!-- ``` -->

<!-- # Make a node -->

<!-- ```haskell -->
<!-- node name per rx tx = monitor name $ do -->
<!--   -- Initialize local state -->
<!--   st <- state (name ++ "_st") -->
<!--   -- Update the state -->
<!--   updateState name st rx -->
<!--   -- Send a new msg -->
<!--   sendMsg name per st tx -->
<!-- ``` -->

<!-- # Action on receiving a message -->

<!-- ```haskell -->
<!-- -- When receiving a message rx, run an callback. -->
<!-- updateState name st rx = -->
<!--   handler rx (name ++ "_recv_msg") $ -->
<!--     callback $ \m -> call_ update m st -->
<!-- ``` -->

<!-- ```c -->
<!-- [ivory| -->
<!-- void update(const * State m, * State st) { -->
<!--    store st as *m+1; -->
<!-- } -->
<!-- |] -->
<!-- ``` -->

<!-- # Action on a clock tick -->

<!-- ```haskell -->
<!-- sendMsg name per st tx = -->
<!--   handler per (name ++ "_tick") $ do -->
<!--     e <- emitter tx 1 -->
<!--     callback $ \_ -> emit e (constRef st) -->
<!-- ``` -->

<!-- # To do -->

<!-- - Add a fault model -->
<!-- - Explore property specification (synchronous observers?) -->
<!-- - Add POSIX backend for simulation -->

