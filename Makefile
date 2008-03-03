all: test

test: test.hs Text/XML/Expat/IO.chs Text/XML/Expat/Tree.hs
	ghc -O2 -idist/build -lexpat -ffi --make -o test test.hs

test-prof: test.hs Text/XML/Expat/IO.chs Text/XML/Expat/Tree.hs
	ghc -prof -auto-all -lexpat -ffi --make -o test-prof test.hs

clean:
	rm -f test prof *.hi *.o
