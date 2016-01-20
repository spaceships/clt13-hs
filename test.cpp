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

bool expect(const string& desc, bool expected, bool recieved) {
    if (expected != recieved) {
        cout << "\033[1;41m";
    }
    cout << desc << recieved;
    if (expected != recieved) {
        cout << "\033[0m";
    }
    cout << endl;
    return expected == recieved;
}

int main()
{
	// PRNG
    clt_state mmap(20, 10, 8, 400, 1);


    encoding x0 = mmap.encode((unsigned long)200);
    encoding x1 = mmap.encode((unsigned long)200);

    // 2 - 2 ?= 0

    cout << mmap.is_zero(x0 - x1) << endl;
    cout << mmap.is_zero(x0) << endl;
    
    return 0;

    ////mpz_class x = 1;
    //mpz_class x = 0;
    //while (x == 0) {
        //x = rand() % mmap.get_modulus();
    //}
    //cout << "x = " << x << endl;

    //// addition test
    //index_set ix;
    //for (unsigned long i = 0; i < num_indices; i++) {
        //ix.insert(i);
    //}
    //encoding x0 = mmap.encode(mpz_class(0), ix);
    //encoding x1 = mmap.encode(mpz_class(0), ix);
    //encoding xp = x0 + x1;
    //ok = expect("is_zero(0 + 0) = ", 1, xp.is_zero());

    //// subtraction test
    //ix.clear();
    //for (unsigned long i = 0; i < num_indices; i++) {
        //ix.insert(i);
    //}
    //x0 = mmap.encode(x, ix);
    //x1 = mmap.encode(x, ix);
    //xp = x0 - x1;
    //ok &= expect("is_zero(x - x) = ", 1, xp.is_zero());

    //// multiplication by zero test
    //ix.clear();
    //for (unsigned long i = 0; i < num_indices - 1; i++) {
        //ix.insert(i);
    //}
    //x0 = mmap.encode(x, ix);
    //ix.clear();
    //ix.insert(num_indices - 1);
    //encoding zero = mmap.encode(mpz_class(0), ix); 
    //xp = x0 * zero; 
    //ok &= expect("is_zero(x * 0) = ", 1, xp.is_zero());

    //// multiplication by one test
    //ix.clear();
    //for (unsigned long i = 0; i < num_indices - 1; i++) {
        //ix.insert(i);
    //}
    //x0 = mmap.encode(x, ix);
    //ix.clear();
    //ix.insert(num_indices - 1);
    //encoding one = mmap.encode(mpz_class(1), ix); 
    //xp = x0 * one; 
    //ok &= expect("is_zero(x * 1) = ", 0, xp.is_zero());

    //return !ok;
}
