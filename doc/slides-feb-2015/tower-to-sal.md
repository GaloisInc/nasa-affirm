% AFFIRM Progress
% Lee Pike; Benjamin Jones; Galois Inc.
% February 11, 2015

# Progress

  * Tower AST
  * Example Tower Code vs. SAL Model
  * Require/Ensure annotations
  * Example SAL Model
  * Concrete Steps

Tower Language
==============

Tower code for a process that updates its state upon receiving
messages.

```haskell
monitor "A" $ do

  st <- state "st"  -- local state
  store st 0        -- initialization

  handler rx "rx" $ do  -- handle channel "rx"
    callback (\m -> update m st)
```

State transition
================

Add received integer to the state.

```haskell
update m st = do
  m' <- deref m
  st' <- deref st
  store st (st' + m')
```

SAL Model
=========

A corresponding SAL specification:

```haskell
monitorA: MODULE =
  INPUT  new? : BOOLEAN
  INPUT  rx   : INTEGER
  LOCAL  st   : INTEGER
  INITIALIZATION
    st = 0
  {- TRANSITION ... -}
END
```

State Transition
================

```haskell
TRANSITION
  [
    new? --> st' = st + rx;
             new?' = FALSE
  []
    ELSE -->
  ]
```

Update Abstracted
=================

Programmer annotates the state machines:

```haskell
callback $ \m ->
  requires (m >= 0) $      -- TODO real syntax
    ensures (st' >= st) $
      update m st = {- original update code-}
```

SAL Transition Abstracted
=========================

```haskell
TRANSITION
  [
    new? AND rx >= 0
         --> st' IN { x : INTEGER | x >= st };
             new?' = FALSE
  []
    new? AND rx < 0
         --> st' IN INTEGER;  -- undefined behavior?
  []
    ELSE -->
  ]
```

Concrete Steps
==============

  * Define Haskell datatypes and mapping to SAL
  * Implement language of constructors and combinators for
    generating SAL syntax
  * Map Tower AST to SAL using the requires/ensures framework
    to handle (Ivory) state machines

