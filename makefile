CC 		= g++
CCFLAGS = -Wall -fdiagnostics-color --std=c++11 -g -O0 -fopenmp #-lrt
IFLAGS 	= -I/opt/local/include 
LFLAGS 	= -L/opt/local/lib -lgmpxx -lgmp

all: src/clt13.cpp src/clt13.hpp src/test.cpp makefile
	$(CC) $(CCFLAGS) $(IFLAGS) -c -o src/clt13.o src/clt13.cpp
	$(CC) $(CCFLAGS) $(IFLAGS) $(LFLAGS) src/test.cpp src/clt13.o -o ctest

clean:
	rm -f src/*.o
	rm -f ctest
