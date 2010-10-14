# Makefile for thing.

simpletest:
	ghc --make -lglm simple_test.hs -o simple_test

clean:
	rm -f *.hi *.o *~ simple_test