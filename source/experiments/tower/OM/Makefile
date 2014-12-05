-include Config.mk

PWD=$(shell pwd)

PACKAGES= \
  $(PREFIX)/ivory/ivory/ \
  $(PREFIX)/ivory/ivory-artifact/ \
  $(PREFIX)/ivory/ivory-stdlib/ \
  $(PREFIX)/ivory/ivory-serialize/ \
  $(PREFIX)/ivory/ivory-opts/ \
  $(PREFIX)/ivory/ivory-backend-c/ \
  $(PREFIX)/tower/tower/ \
  $(PREFIX)/tower/tower-aadl/

all: build

.PHONY: build
build:
	cabal build

.PHONY: add-srcs
add-srcs:
	cabal sandbox init
	cabal sandbox add-source $(PACKAGES)
	cabal install $(PACKAGES)

.PHONY: veryclean
veryclean:
	-rm -rf dist
	-rm -rf .cabal-sandbox
	-rm -rf cabal.sandbox.confg
