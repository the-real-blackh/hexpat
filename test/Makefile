all: tests lazySAX lazyTree benchmark leak

tests: tests.hs
	ghc -O2 -lexpat --make -o tests tests.hs

perf: test.hs
	ghc -O2 -lexpat --make -o perf perf.hs

lazySAX: lazySAX.hs
	ghc -O2 -lexpat --make -o lazySAX lazySAX.hs

lazyTree: lazyTree.hs
	ghc -O2 -lexpat --make -o lazyTree lazyTree.hs

benchmark: benchmark.hs
	ghc -O2 -lexpat --make -o benchmark benchmark.hs

leak: leak.hs
	ghc -O2 -lexpat --make -o leak leak.hs

clean:
	rm -f tests perf lazySAX lazyTree benchmark *.hi *.o
