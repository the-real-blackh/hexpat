all: test test2

test: test.hs Text/XML/Expat/IO.chs Text/XML/Expat/Tree.hs
	ghc -O2 -idist/build -lexpat -ffi --make -o test test.hs

test2: test2.hs Text/XML/Expat/IO.chs Text/XML/Expat/Tree.hs
	ghc -O2 -idist/build -lexpat --make -o test2 test2.hs

test-prof: test.hs Text/XML/Expat/IO.chs Text/XML/Expat/Tree.hs
	ghc -prof -auto-all -lexpat -ffi --make -o test-prof test.hs

clean:
	rm -f test prof *.hi *.o
