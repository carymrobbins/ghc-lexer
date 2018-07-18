.PHONY: compile run clean

compile:
	stack build

run: compile
	stack exec ghc-lexer

clean:
	stack clean
