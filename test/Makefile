all: tests lazySAX lazyTree benchmark leak

tests: tests.hs
	ghc -O2 -lexpat --make -o tests tests.hs -threaded

perf: test.hs
	ghc -O2 -lexpat --make -o perf perf.hs -threaded

lazySAX: lazySAX.hs
	ghc -O2 -lexpat --make -o lazySAX lazySAX.hs -threaded

lazyTree: lazyTree.hs
	ghc -O2 -lexpat --make -o lazyTree lazyTree.hs -threaded

benchmark: benchmark.hs
	ghc -O2 -lexpat --make -o benchmark benchmark.hs -threaded

leak: leak.hs
	ghc -O2 -lexpat --make -o leak leak.hs -threaded

clean:
	rm -f tests perf lazySAX lazyTree benchmark leak *.hi *.o
