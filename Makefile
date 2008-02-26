
test: test.hs
	ghc -lexpat -ffi -idist/build --make -o test test.hs
