-include Config.mk

PWD=$(shell pwd)

PACKAGES= \
  $(PREFIX)/ivory \
  $(PREFIX)/ivory-stdlib \
  $(PREFIX)/ivory-serialize \
  $(PREFIX)/ivory-opts \
  $(PREFIX)/ivory-backend-c \
  $(PWD)/tower/tower-freertos \
  $(PWD)/tower/tower

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
