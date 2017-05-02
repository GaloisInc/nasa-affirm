% AFFIRM Progress
% Benjamin Jones & Lee Pike
% _Galois Inc._, April 7, 2015

Ivory to SAL
============

Generating logical specifications from an architectural DSL
-----------------------------------------------------------

 * We are exploring compilation of Tower/Ivory (ADSL) specified systems to
   transition systems in SAL, suitable for model checking.

* We've discussed architectural aspects of Tower and how they naturally map to
  SAL.

 * __Callbacks__ associated to Tower nodes are Ivory procedures, in general. This
   month we've been experimenting around how these imperative procedures can
   be mapped to SAL.

Ivory Procedure
===============

In our OM(1) example we have this (simplified) Ivory code that performs a
majority vote over an input array:

```haskell
iVote arr = do
  -- stack local vars
  c   <- local izero    -- current candidate
  cnt <- local izero    -- candidate counter

  -- loop over input array
  arrayMap $ \ix -> do
    x   <- deref (arr ! ix)

  -- ...
```

Ivory Procedure
===============

```haskell
    -- ...
    cond_
      [ (x ==? c)       -- we see current candidate:
      ==> (cntR += 1)   --   increment counter
      , cnt ==? 0       -- current candidate has counter=0:
      ==> do store c x  --   store a new candidate
             store cnt 1
      , true            -- else:
      ==> cntR += (-1)  --   decrement counter
      ]
  return =<< c
```

Ivory Procedure
===============

Modeling this procedure faithfully in SAL presents several challenges:

  * it has local state that is mutated
  * it is given as an imperative loop

This makes modeling a procedure like `iVote` as a SAL expression (RHS of a transition
relation), or pure function application, difficult in general.

Ivory Procedures as Machines
============================

One possibility is to map Ivory procedures like `iVote` to finite state
machines in SAL:

  * machine maintains the local state specified in Ivory
  * machine maintains auxiliary local state (e.g. program counters, status,
    call stack, ...)
  * Tower nodes "call" the procedure by sending a message to the procedure's
    machine and waiting for a response

`iVote` Machine
===============

Walk through SAL model...
