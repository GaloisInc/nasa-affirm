ATOMOM1 := ../.cabal-sandbox/bin/atom-om1

all: om1
.PHONY: all

om1.c om1.h:
	$(ATOMOM1)

om1: om1.c om1.h
	clang -o om1 om1.c

clean:
	rm om1 om1.c om1.h om1.o
