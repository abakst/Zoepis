# Makefile for thing.

HC=ghc
HCFLAG=--make
HLFLAG=-lglm
DO=$(HC) $(HCFLAG) $(HLFLAG)

basic_test:
	$(DO) basic_test.hs -o basic_test

clean:
	rm -f *.hi *.o *~ basic_test
