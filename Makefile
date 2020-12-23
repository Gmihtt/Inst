.PHONY: fmt

fmt: 
	cd src && ormolu --mode inplace $$(find . -name '*.hs')