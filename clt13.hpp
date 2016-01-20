#ifndef __CLT13_H
#define __CLT13_H

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <gmpxx.h>
#include <assert.h>
#include <string>
#include <sys/time.h>
#include <cmath>

//#define kappa 6					// Maximal level
//#define hBits 80 				// size of h_i's in v = BETA
//#define ell 160 				// number of elements during encoding
//#define theta  15 				// number of non-zero elements in subset sum during rerand 

//#define sessionKeyBits 160 		// Bitsize of session key to derive
//#define bound sessionKeyBits 	// bound to decide if it is zero or not
								//// bound must be >= sessionKeyBits
//#define alpha 80				// size of g_i's and elements of A

//#if INSTANTIATION == 1 		// Small

	//#define N  540  			// number of p_i's
	//#define delta 23 			// sqrt(N)
	//#define eta 1838			// size of p_i's
	//#define etp 460				// Size of primes in p_i's; it is better to have eta%etp=0
	//#define rho 41				// size of r_i's in xp_i's and y

//#elif INSTANTIATION == 2 		// Medium

	//#define N  2085  			// number of p_i's
	//#define delta 45 			// sqrt(N)
	//#define eta 2043			// size of p_i's
	//#define etp 409				// Size of primes in p_i's; it is better to have eta%etp=0
	//#define rho 56				// size of r_i's in xp_i's and y

//#elif INSTANTIATION == 3 		// Large

	//#define N  8250  			// number of p_i's
	//#define delta 90 			// sqrt(N)
	//#define eta 2261			// size of p_i's
	//#define etp 453				// Size of primes in p_i's; it is better to have eta%etp=0
	//#define rho 72				// size of r_i's in xp_i's and y

//#elif INSTANTIATION == 4 		// Extra

	//#define N  26115  			// number of p_i's
	//#define delta 161 			// sqrt(N)
	//#define eta 2438			// size of p_i's
	//#define etp 407				// Size of primes in p_i's; it is better to have eta%etp=0
	//#define rho 85				// size of r_i's in xp_i's and y

//#endif



double currentTime();

class clt_state;

/* 
Class encoding

Contain: 
- ciphertext value (large integer) `cval'
- ciphertext degree `degree'
- pointer to the key associated with the ciphertext `key'
*/
class encoding {
private:
	mpz_class cval;
	long degree;
	clt_state* key;

public:
	encoding();
	encoding(const encoding& c);
	encoding(clt_state* mmkey, mpz_class c, unsigned long deg);

	unsigned long get_noise();

	unsigned long get_degree() const {return degree;};
	mpz_class get_cval() const {return cval;};

	encoding& operator =(const encoding&);
	encoding& operator+=(const encoding&);
	encoding& operator*=(const encoding&);
	encoding& operator-=(const encoding&);
	
	encoding operator+(const encoding& c) const {
	    encoding c2(*this);
	    return (c2 += c);
	}

	encoding operator-(const encoding& c) const {
	    encoding c2(*this);
	    return (c2 -= c);
	}

	encoding operator*(encoding& c) const {
	    encoding c2(*this);
	    return (c2 *= c);
	}
};

/* 
Class clt_state (Multilinear-Map)

Contain: 
- pointer to the gmp pseudorandom generator `rng'
- pointer to secret primes `p'
- public key value `x0' (=prod(p))
- private value `z' and `zkappa'=z^kappa
- private value `zinv' = z^(-1) mod x0
- pointer to private elements `g'
- pointer to secret matrix `A'
- pointer to public values `xp'
- public value `y'
- public zero-tester `pzt'
- pointer to rerandomization values `varpi' (= x in the article)
*/
class clt_state {
private:
	gmp_randclass* 	rng;
	mpz_class* 		p; 				//	[N];
	mpz_class 		x0, z, zkappa;
	mpz_class 		zinv; 			//	[N];
	mpz_class* 		crtCoeff; 		//	[N];
	mpz_class* 		g; 				//	[N];
	mpz_class 		pzt;
	mpz_class 		y;

public:
    unsigned long secparam;
    unsigned long n;
    unsigned long nzs;
    unsigned long rho;
    unsigned long nu;
    unsigned long kappa;
    unsigned long beta;

    clt_state
    ( 
        gmp_randclass* random,
        unsigned long secparam,
        unsigned long kappa,
        unsigned long nzs,
        unsigned long etap,
        int verbose = 0
    );

	~clt_state();
    encoding Encrypt(unsigned long m);
	mpz_class Encrypt_with_sk(mpz_class* m, unsigned long nbBits, unsigned long degree);
	mpz_class Encrypt_with_sk(unsigned long m, unsigned long nbBits, unsigned long degree);
	mpz_class reduce(const mpz_class &c);
	unsigned long get_noise(const mpz_class& c, unsigned long degree);
	mpz_class zero_test(const mpz_class &c, unsigned long degree);
	unsigned long nbBits(const mpz_class &v);
	bool is_zero(const encoding &c);
	mpz_class& get_x0() { return x0; };
};

#endif 
// #ifndef __MULTIMAP_H
