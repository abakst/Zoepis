# Makefile for thing.

HC=ghc
HCFLAG=--make -O3 -outputdir ./bin -funbox-strict-fields
HLFLAG=-lglm -ljpeg -lpng -threaded

ifeq ($(PROFILE),1)
HCFLAG += -prof -auto-all
endif

DO=$(HC) $(HCFLAG) $(HLFLAG)

.PHONY: basic_test
basic_test:
	$(DO) basic_test.hs -o basic_test

run_test: basic_test
	./basic_test

clean:
	rm -rf *.hi *.o *~ basic_test bin
