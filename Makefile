.PHONY: compile run clean

compile:
	stack build ghc-lexer

run: compile
	stack exec ghc-lexer

clean:
	stack clean

install:
	stack install ghc-lexer
