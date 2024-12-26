
C       = g++
CFLAGS  = -g -c -shared -fPIC

LIBFILE = libCacBDD.a

libCacBDD.a:
	$(C) $(CFLAGS) -I../cpp -o libBDDNodeC.so BDDNodeC.cpp
	rm -f $(LIBFILE)
	ar rvs $(LIBFILE) libBDDNodeC.so
	ar rvs $(LIBFILE) ../cpp/*.o

clean:
	rm -f *.a *.o *.so *.gch

ctest: main.c libCacBDD.a
	$(C) -g -O3 -Wno-deprecated main.c -o ctest -L. -lm -lCacBDD -I../cpp
