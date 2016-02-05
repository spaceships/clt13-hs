CC 		= g++
CCFLAGS = -Wall --std=c++11 -g -O0 -fopenmp #-lrt
IFLAGS 	= -I/opt/local/include -Isrc/
LFLAGS 	= -L/opt/local/lib -lgmpxx -lgmp

OBJS = src/clt_mlm.o src/utils.o 
SRCS = src/clt_mlm.cpp src/utils.cpp 

test.cpp: $(OBJS) $(SRCS)
	$(CC) $(CCFLAGS) $(IFLAGS) $(LFLAGS) $(OBJS) src/test_mlm.cpp -o ctest

src/%.o: src/%.cpp
	$(CC) $(CCFLAGS) $(IFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f test_mlm
