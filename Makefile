.PHONY: all build run clean

all: build

build:
	cabal build

run:
	cabal run

clean:
	cabal clean
