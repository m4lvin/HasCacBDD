.SUFFIXES: .o .cc .u

CPP	= g++
CFLAGS = -g -O3 -Wno-deprecated -shared -fPIC

default:
	$(CPP) -c UTable.cpp  $(CFLAGS)
	$(CPP) -c Manager.cpp $(CFLAGS)
	$(CPP) -c CTable.cpp  $(CFLAGS)
	$(CPP) -c BDDNode.cpp $(CFLAGS)
	$(CPP) -c DdNode.cpp  $(CFLAGS)

clean:
	rm -f *.o
