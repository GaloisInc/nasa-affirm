# atom-om1

[Atom](https://github.com/tomahawkins/atom) (Haskell eDSL) specification for a
concurrent [OM1](link) running on a shared memory distributed system.


# Usage

Compile the executable:

```
% cabal sandbox init
% cabal install --only-dependencies
% cabal install
```

Then, running the executable will generate `om1.{c,h}` and print out the
compiled schedule.
