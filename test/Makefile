all: tests lazySAX lazyTree lazyTreeThrow benchmark leak helloworld locations

tests: tests.hs
	ghc -O2 --make -o tests tests.hs

lazySAX: lazySAX.hs
	ghc -O2 --make -o lazySAX lazySAX.hs

lazyTree: lazyTree.hs
	ghc -O2 --make -o lazyTree lazyTree.hs

lazyTreeThrow: lazyTreeThrow.hs
	ghc -O2 --make -o lazyTreeThrow lazyTreeThrow.hs

benchmark: benchmark.hs
	ghc -O2 --make -o benchmark benchmark.hs #-threaded

leak: leak.hs
	ghc -O2 --make -o leak leak.hs -threaded

helloworld: helloworld.hs
	ghc -O2 --make -o helloworld helloworld.hs

locations: locations.hs
	ghc -O2 --make -o locations locations.hs

clean:
	rm -f tests perf lazySAX lazyTree benchmark leak helloworld locations *.hi *.o
