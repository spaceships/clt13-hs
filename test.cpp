#include <iostream>
#include <cstdlib>
#include <gmpxx.h>
#include <assert.h>
#include <sys/time.h>
#include "clt13.hpp"

#define LINUX false			// Timings using CLOCK_MONOTONIC
#define VERBOSE true			// Display additional information
#define DISPLAY_MESSAGES true		// Decrypt & display the messages (only if VERBOSE==true)

#define USERS kappa+1			// Number of Users: make sure (USERS-1) <= kappa

using namespace std;

int main()
{
	// PRNG
	gmp_randclass* random = new gmp_randclass(gmp_randinit_default);
    clt_state mmap(random, 19, 10, 8, 400, 1);


    encoding x0 = mmap.Encrypt((unsigned long)2);
    encoding x1 = mmap.Encrypt((unsigned long)2);

    // 2 - 2 ?= 0

    cout << mmap.is_zero(x0 - x1) << endl;
    cout << mmap.is_zero(x0) << endl;
    
    return 0;
}
