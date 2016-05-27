ATOMOM1 := .cabal-sandbox/bin/atom-om1

all: om1
.PHONY: all

om1: om1.c om1.h
	clang -o om1 om1.c

om1.c om1.h: AtomOM1.hs $(ATOMOM1)
	$(ATOMOM1)

$(ATOMOM1): AtomOM1.hs
	cabal install

clean:
	rm om1 om1.c om1.h
