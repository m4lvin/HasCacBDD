
HS      = ghc
HSFLAGS = -lstdc++ -Wall -fno-cse

CPP     = g++
CFLAGS  = -g -c -shared -fPIC

CABALFLAGS = --with-gcc=g++

LIBDIR = /usr/local/cacbdd/
LIBFILE = libBDDNodeC.a

default:
	@echo "To install CacBDD to $(LIBDIR) and then cabal install Data.HasCacBDD, use 'make all'."
	@echo "For more details look at the Makefile."

cppbuild:
	cd cpp && make

cbuild:
	$(CPP) $(CFLAGS) -Icpp -o dist/libBDDNodeC.so c/BDDNodeC.cpp

cinstall:
	mkdir -p $(LIBDIR)
	rm -f $(LIBDIR)$(LIBFILE)
	ar rvs $(LIBDIR)$(LIBFILE) dist/libBDDNodeC.so
	ar rvs $(LIBDIR)$(LIBFILE) cpp/*.o

hsbuild:
	cabal configure $(CABALFLAGS)
	cabal build $(CABALFLAGS)

hsinstall:
	cabal install $(CABALFLAGS)

test:
	$(HS) $(HSFLAGS) test.hs -o dist/test
	dist/test

clean:
	cd cpp && make cla
	rm -rf dist/*
	rm -f c/*.o
	rm -f c/*.so
	rm -f *.hi
	rm -f *.o

all:
	make cppbuild
	make cbuild
	sudo make cinstall
	make hsbuild
	make hsinstall
