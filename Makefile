
.PHONY: build run debug debugbytes test doc clean port

build:
	stack build

run: port
	stack run -- -p $(TCP_PORT)

debug: port
	stack run -- -p $(TCP_PORT) -v 7

debugbytes: port
	stack run -- -p $(TCP_PORT) -v 8

test:
	stack test --cabal-verbose

# This puts the docs somewhere deep inside .stack-work.
# See the output for the actual path.
doc:
	stack haddock

clean:
	stack clean


port:
ifndef TCP_PORT
	$(error TCP_PORT is not defined)
endif
