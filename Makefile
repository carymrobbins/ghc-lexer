.PHONY: compile run clean

compile:
	stack build

run: compile
	stack exec ghc-parser

clean:
	stack clean
