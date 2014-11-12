% Tower Overview
% Lee Pike
% Nov 2014

# Concepts

- System
    - Set of channels and architecture
    - Clocks
- Monitor
    - Local state
    - A set of handlers
- Handler
    - Triggered by
        - A clock event
        - A channel event
        - A hardware event
    - Initiates a callback
- Callback
    - State-machine that implements behavior when the handler fires

# 3-Node Distributed System

- Three synchronous nodes
- Each sending its state (an Int) to it's right neighbor
- Then incrementing its state by 1.

```
    -----------------------------------
    |                                 |
    |-- > node0 --> node1 --> node2 --|
```

# Make the system

```haskell
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
  node "nod0" per out2 in0
  node "nod1" per out0 in1
  node "nod2" per out1 in2
```

# Make a node

```haskell
node name per rx tx = monitor name $ do
  -- Initialize local state
  st <- state (name ++ "_st")
  -- Update the state
  updateState name st rx
  -- Send a new msg
  sendMsg name per st tx
```

# Action on receiving a message

```haskell
-- When receiving a message rx, run an callback.
updateState name st rx =
  handler rx (name ++ "_recv_msg") $
    callback $ \m -> call_ update m st
```

```c
[ivory|
void update(const * State m, * State st) {
   store st as *m+1;
}
|]
```

# Action on a clock tick

```haskell
sendMsg name per st tx =
  handler per (name ++ "_tick") $ do
    e <- emitter tx 1
    callback $ \_ -> emit e (constRef st)
```

# To do

- Add a fault model
- Explore property specification (synchronous observers?)
- Add POSIX backend for simulation

