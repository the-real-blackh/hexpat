
test: test.hs Raw.hs
	ghc -fffi --make test.hs -o test -lexpat

Raw.hs: Raw.chs
	~/.cabal/bin/c2hs -l Raw.chs

clean:
	rm -f *.hi *.o *.chi *.chs.h *.c *.h
