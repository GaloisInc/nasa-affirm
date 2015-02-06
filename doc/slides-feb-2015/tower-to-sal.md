# AFFIRM Feb 2015

# Topics

  * Recall Tower
  * Example Tower Model vs. SAL Model
  * Require/Ensure annotations
  * Example SAL Model
  * Concrete Steps

# Tower Language

```haskell
(in0, out0) <- channel      -- out0 is rx for "A"
(in1, out1) <- channel      -- in1  is tx for "A"
clk         <- period (Microseconds 1000)

monitor "A" $ do

  st <- state "A_st"        -- state reference
  store st 0

  handler out0 "recv" $ do  -- update state upon recv
    callback (\m -> update m st)

  handler clk "send"  $ do  -- send current state on clock
    e <- emitter in1 1
    callback (\_ -> emit e (constRef st))
```

# State transition

```haskell
-- update 'st' by adding 'm' to it
update m st = do
  m' <- deref m
  st' <- deref st
  store st (st' + m')
```

# SAL Model

```haskell
monitorA: MODULE =
  INPUT  recv    : BOOLEAN  -- new msg?
  INPUT  out0    : INTEGER  -- msg
  INPUT  clk     : CLOCK    -- { low, pos_edge, high, neg_edge }
  OUTPUT in1     : INTEGER
  LOCAL  state   : INTEGER
  INITIALIZATION
    state = 0
  {- TRANSITION ... -}
END
```

# Transition

```haskell
TRANSITION                -- async composition of the handlers
  [
    clk = pos_edge      --> in1' = state
  []
    recv AND out0 >= 0  --> state' = state + out0;
                            recv' = FALSE
  []
    ELSE                -->
  ]
```
