-include Config.mk

PWD=$(shell pwd)

PACKAGES= \
  $(PREFIX)/ivory/ivory/ \
  $(PREFIX)/ivory/ivory-artifact/ \
  $(PREFIX)/ivory/ivory-backend-c/ \
  $(PREFIX)/ivory/ivory-stdlib/ \
  $(PREFIX)/ivory/ivory-serialize/ \
  $(PREFIX)/ivory/ivory-opts/ \
  $(PREFIX)/tower/tower/ \
  $(PREFIX)/tower/tower-config \
  $(PREFIX)/tower/tower-aadl/ \
  $(PREFIX)/tower/tower-hal/ \
  $(PREFIX)/ivory-tower-posix/

all: add-srcs

run_om:
	cabal run aadl  -- --src-dir=om_aadl
	cabal run posix -- --src-dir=om_posix

.PHONY: add-srcs
add-srcs:
	cabal sandbox init
	cabal sandbox add-source $(PACKAGES)
	cabal install --dependencies-only

.PHONY: clean
clean:
	-rm -rf AADL

.PHONY: veryclean
veryclean: clean
	-rm -rf dist
	-rm -rf .cabal-sandbox
	-rm -rf cabal.sandbox.config
