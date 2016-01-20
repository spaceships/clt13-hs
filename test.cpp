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

std::ostream& operator<<(std::ostream& os, Ciphertext& c) {
    os << "<Ciphertext of degree=" << c.get_degree();

    #if VERBOSE
    os << " and with bit-noise=" << c.get_noise() << "/" << (eta-alpha);
	
	#if DISPLAY_MESSAGES
	unsigned i;
	mpz_class* m;
	m = new mpz_class[N];
  	c.Decrypt_with_sk(m);
	os << std::endl;
	os << "          m=(";
    for (i=0; i<N; i++)
    	os << m[i] << ((i==(N-1))?") ":" ");
    #endif

    #endif 
    // #if VERBOSE
    os << ">";
    return os;
}

void tobv(unsigned long inp, bool *res) {
  unsigned long mangle = inp;
  for (int i = 0; i < ell; i++) {
    if (mangle > 0) {
      res[i] = mangle & 1;
      mangle >>= 1;
    } else {
      res[i] = 0;
    }
  }
}

unsigned long frombv(mpz_class* inp) {
  unsigned long ret = 0;
  for (unsigned int i = sizeof(unsigned long) * 8; i > 0; i--) {
    if (inp[i] > 0) ret += 1;
    ret <<= 1;
  }
  return ret;
}

int main()
{
	// PRNG
	gmp_randclass* random = new gmp_randclass(gmp_randinit_default);
	MMKey key(random);

    Ciphertext x0 = key.Encrypt((unsigned long)2);
    Ciphertext x1 = key.Encrypt((unsigned long)2);

    // 2 - 2 ?= 0

    cout << key.is_zero(x0 - x1) << endl;
    cout << key.is_zero(x0) << endl;
    
    return 0;
}
