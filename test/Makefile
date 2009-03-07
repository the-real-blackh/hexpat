all: tests lazySAX lazyTree

tests: tests.hs
	ghc -O2 -lexpat --make -o tests tests.hs

perf: test.hs
	ghc -O2 -lexpat --make -o perf perf.hs

lazySAX: lazySAX.hs
	ghc -O2 -lexpat --make -o lazySAX lazySAX.hs

lazyTree: lazyTree.hs
	ghc -O2 -lexpat --make -o lazyTree lazyTree.hs

clean:
	rm -f test test2 perf lazySAX lazyTree *.hi *.o Text/XML/Expat/*.o
