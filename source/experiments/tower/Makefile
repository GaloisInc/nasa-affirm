-include Config.mk

PWD=$(shell pwd)

PACKAGES= \
  $(PREFIX)/ivory/ivory/ \
  $(PREFIX)/ivory/ivory-artifact/ \
  $(PREFIX)/ivory/ivory-stdlib/ \
  $(PREFIX)/ivory/ivory-serialize/ \
  $(PREFIX)/ivory/ivory-opts/ \
  $(PREFIX)/ivory/ivory-backend-c/ \
  $(PREFIX)/tower/tower/

all: build

.cabal-sandbox:
	cabal sandbox init

.PHONY: add-srcs
add-srcs: .cabal-sandbox
	cabal sandbox add-source $(PACKAGES)

.PHONY: build
build: add-srcs
	cabal install $(PACKAGES)

.PHONY: veryclean
veryclean:
	-rm -rf .cabal-sandbox
	-rm -rf cabal.sandbox.confg
