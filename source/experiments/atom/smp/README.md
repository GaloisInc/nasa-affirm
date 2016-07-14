# atom-smp

A specification in [Atom](https://github.com/tomahawkins/atom) (Haskell eDSL) for a
concurrent simple message passing system. The system has two nodes. One node,
the "source", sends a single message to another node, the "receiver".


# Usage

Compilation requires GHC, cabal, and a C99 compatible C compiler.

To compile the code generator, run it (producing .c and .h files) and
compile the C code all at once, run:

```
% make
```

To compile the code generator alone, run:

```
% cabal sandbox init
% cabal install --only-dependencies
% cabal install
```

Then, running the executable will generate `smp.{c,h}` and print out the
compiled schedule.
