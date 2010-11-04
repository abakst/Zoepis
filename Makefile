# Makefile for thing.

HC=ghc
HCFLAG=--make -O2 -outputdir ./bin
HLFLAG=-lglm -ljpeg -lpng -threaded
DO=$(HC) $(HCFLAG) $(HLFLAG)

.PHONY: basic_test
basic_test:
	$(DO) basic_test.hs -o basic_test

clean:
	rm -rf *.hi *.o *~ basic_test bin
