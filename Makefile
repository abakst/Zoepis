# Makefile for thing.

HC=ghc
HCFLAG=--make
HLFLAG=-lglm
DO=$(HC) $(HCFLAG) $(HLFLAG)

simple_test:
	$(DO) simple_test.hs -o simple_test

basic_test:
	$(DO) basic_test.hs -o basic_test

clean:
	rm -f *.hi *.o *~ simple_test basic_test
