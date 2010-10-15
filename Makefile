# Makefile for thing.

simpletest:
	ghc --make -lglm simple_test.hs -o simple_test

basictest:
	ghc --make -lglm basic_test.hs -o basic_test
clean:
	rm -f *.hi *.o *~ simple_test basic_test
