
C       = g++
CFLAGS  = -g -c -shared -fPIC

LIBDIR = /usr/local/cacbdd/
LIBFILE = libBDDNodeC.a

default:
	@echo "You probably want to type 'make all' which will"
	@echo "1) Download CacBDD from http://www.kailesu.net/CacBDD/CacBDD1.01.zip"
	@echo "2) Install a C-wrapped version of CacBDD to $(LIBDIR)"
	@echo "3) Install Data.HasCacBDD  with cabal."
	@echo "For more details look at the Makefile."

getcpp:
	mkdir -p ./dist/cpp
	if [ ! -f ./dist/cpp/BDDNode.h ] ; \
	then wget -c http://www.kailesu.net/CacBDD/CacBDD1.01.zip -O ./dist/CacBDD1.01.zip ; \
	unzip -n ./dist/CacBDD1.01.zip -d ./dist/cpp/ ; \
	patch ./dist/cpp/Makefile ./CacBDD-Makefile.patch ; \
	fi ;

cppbuild:
	cd ./dist/cpp && make

cbuild:
	$(C) $(CFLAGS) -Idist/cpp -o dist/libBDDNodeC.so c/BDDNodeC.cpp

cinstall:
	mkdir -p $(LIBDIR)
	rm -f $(LIBDIR)$(LIBFILE)
	ar rvs $(LIBDIR)$(LIBFILE) dist/libBDDNodeC.so
	ar rvs $(LIBDIR)$(LIBFILE) dist/cpp/*.o

hsbuild:
	stack build

example:
	stack build
	stack exec hascacbdd-example

clean:
	rm -rf dist/*

allnonhaskell:
	make getcpp
	make cppbuild
	make cbuild
	sudo make cinstall

all:
	make getcpp
	make cppbuild
	make cbuild
	sudo make cinstall
	make hsbuild
