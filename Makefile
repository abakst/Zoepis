# Makefile for thing.

HC=ghc
HCFLAG=--make -outputdir ./bin -funbox-strict-fields -O3
HLFLAG=-lglm -ljpeg -lpng -threaded

ifeq ($(PROFILE),1)
HCFLAG += -prof -auto-all
endif

DO=$(HC) $(HCFLAG) $(HLFLAG)

.PHONY: zoepis

zoepis: 
	$(DO) Zoepis.hs

conway:
	$(DO) conway.hs -o conway

basic_test:
	$(DO) basic_test.hs -o basic_test

run_%: %
	./$*

run_test: basic_test
	./basic_test

clean:
	rm -rf *.hi *.o *~ *.aux *.hp *.prof *.ps basic_test bin
