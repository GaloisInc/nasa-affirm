ATOM_LOC := $(HOME)/git/atom
ATOM_SMP := .cabal-sandbox/bin/atom-smp

all: smp
.PHONY: all

sandbox:
	cabal sandbox init
	cabal sandbox add-source $(ATOM_LOC)

smp: smp.c smp.h
	$(CC) -o smp smp.c

smp.c smp.h: AtomSMP.hs $(ATOM_SMP)
	$(ATOM_SMP)

$(ATOM_SMP): AtomSMP.hs
	cabal install

clean:
	rm smp smp.c smp.h
