
HS      = ghc
HSFLAGS = -O0 -lstdc++ -Wall -fno-cse

CPP     = g++
CFLAGS  = -g -c -shared -fPIC -O0

CABALFLAGS = --with-gcc=g++

default:
	echo "Please select steps by hand."

cppbuild:
	cd cpp && make

cppinstall:
	mkdir -p /usr/local/cacbdd
	cp cpp/*.o /usr/local/cacbdd
	cp cpp/*.h /usr/local/cacbdd

cbuild:
	$(CPP) $(CFLAGS) -I/usr/local/cacbdd -o c/libBDDNodeC.so c/BDDNodeC.cpp

cinstall:
	cp c/* /usr/local/cacbdd

hsbuild:
	cabal configure $(CABALFLAGS)
	cabal build $(CABALFLAGS)

hsinstall:
	cabal install $(CABALFLAGS)

testlocal:
	$(HS) $(HSFLAGS) test.hs -o dist/test cpp/Manager.o cpp/BDDNode.o cpp/DdNode.o cpp/UTable.o cpp/CTable.o
	dist/test

testinstalled:
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
	make cppinstall
	make cbuild
	make cinstall
	make hsbuild
	make hsinstall

cuninstall:
	rm -rf /usr/local/cacbdd
