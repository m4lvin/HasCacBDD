
C       = g++
CFLAGS  = -g -c -shared -fPIC

LIBFILE = libCacBDD.a

default:
	$(C) $(CFLAGS) -I../cpp -o libBDDNodeC.so BDDNodeC.cpp
	rm -f $(LIBFILE)
	ar rvs $(LIBFILE) libBDDNodeC.so
	ar rvs $(LIBFILE) ../cpp/*.o

clean:
	rm -f *.a *.o *.so *.gch
