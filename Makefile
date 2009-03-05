all: test test2

test: test.hs 
	ghc -O2 -lexpat --make -o test test.hs

test2: test2.hs
	ghc -O2 -lexpat --make -o test2 test2.hs

perf: test.hs
	ghc -O2 -lexpat --make -o perf perf.hs

clean:
	rm -f test test2 perf prof *.hi *.o Text/XML/Expat/*.o
