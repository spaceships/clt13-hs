CC 		= g++
CCFLAGS = -Wall -fdiagnostics-color --std=c++11 -g -O0 -fopenmp #-lrt
IFLAGS 	= -I/opt/local/include 
LFLAGS 	= -L/opt/local/lib -lgmpxx -lgmp

all: clt13.cpp clt13.hpp test.cpp makefile
	$(CC) $(CCFLAGS) $(IFLAGS) -c -o clt13.o clt13.cpp
	$(CC) $(CCFLAGS) $(IFLAGS) $(LFLAGS) test.cpp clt13.o -o test

clean:
	rm -f *.o
	rm -f test
