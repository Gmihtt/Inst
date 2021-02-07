.PHONY: fmt build run stopSctipt buildHS buildJS deleteLog

fmt: 
	cd src && ormolu --mode inplace $$(find . -name '*.hs')

buildHS:
	stack build --ghc-options="-threaded -Wall -O2 -Werror=missing-fields -Werror=incomplete-record-updates"

buildJS:
	cd src/JS/inst/ && npm run compile

stopScript:
	screen -ls | grep '(Detached)' | awk '{print $$1}' | xargs -I % -t screen -X -S % quit

build: stopScript buildHS buildJS

deleteLog:
	rm -f logging.log

run: deleteLog build
	./instStart.sh

