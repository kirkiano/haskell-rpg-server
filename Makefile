
build:
	stack build

run:
	stack exec rpg-server-engine-exe -- -p $(TCP_PORT)

debug:
	stack exec rpg-server-engine-exe -- -d -p $(TCP_PORT)

# This puts the docs somewhere deep inside .stack-work.
# See the output for the actual path.
doc:
	stack haddock

clean:
	stack clean


ifndef TCP_PORT
	$(error TCP_PORT is not defined)
endif
