# Makefile for thing.

HC=ghc
HCFLAG=--make -O2
HLFLAG=-lglm -ljpeg -lpng -threaded
DO=$(HC) $(HCFLAG) $(HLFLAG)

.PHONY: basic_test
basic_test:
	$(DO) basic_test.hs -o basic_test

clean:
	rm -f *.hi *.o *~ basic_test
