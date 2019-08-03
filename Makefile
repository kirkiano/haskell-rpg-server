
build:
	stack build

run:
	stack exec rpg-server-engine-exe

debug:
	stack exec rpg-server-engine-exe -- -d

# This puts the docs somewhere deep inside .stack-work.
# See the output for the actual path.
doc:
	stack haddock

clean:
	stack clean
