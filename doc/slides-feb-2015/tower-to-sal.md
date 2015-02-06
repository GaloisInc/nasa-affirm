# AFFIRM Progress Feb. 2015

Topics:

  * Recall Tower
  * Example Tower AST vs. SAL AST
  * Require/Ensure annotations
  * Example SAL Model
  * Concrete Steps

# Recall Tower Language

```haskell
(in0, out0) <- channel      -- out0 is rx for "A"
(in1, out1) <- channel      -- in1  is tx for "A"
clk <- period (Microseconds 1000)

monitor "A" $ do

  st <- state "A_st"        -- state reference

  handler out0 "recv" $ do  -- update state upon recv
    callback (\m -> call_ update m st)

  handler clk "send"  $ do  -- send msg on clock
    e <- emitter in1 1
    callback (\_ -> emit e (constRef st))
```

