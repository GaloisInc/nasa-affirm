ATOMSMP := .cabal-sandbox/bin/atom-smp

all: smp
.PHONY: all

smp: smp.c smp.h
	$(CC) -o smp smp.c

smp.c smp.h: AtomSMP.hs $(ATOMSMP)
	$(ATOMSMP)

$(ATOMSMP): AtomSMP.hs
	cabal install

clean:
	rm smp smp.c smp.h
