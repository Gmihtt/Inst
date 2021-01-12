.PHONY: fmt build run

fmt: 
	cd src && ormolu --mode inplace $$(find . -name '*.hs')

build:
	stack build --ghc-options="-Wall -Werror"

run: build
	stack run