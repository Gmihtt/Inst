.PHONY: fmt build run

fmt: 
	cd src && ormolu --mode inplace $$(find . -name '*.hs')

build:
	stack build --ghc-options="-threaded -Wall -O2 -Werror=missing-fields -Werror=incomplete-record-updates"

run: build
	./instStart.sh