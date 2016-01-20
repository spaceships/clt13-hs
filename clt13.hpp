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
#include <set>
#include <vector>

using std::vector;

typedef std::set<unsigned long> index_set;

vector<index_set> exclusive_partition_family(unsigned lambda, unsigned d, unsigned i, unsigned start = 0);
index_set index_union (const index_set &x, const index_set &y);
bool distinct_indices (const index_set &x, const index_set &y);

double currentTime();

class clt_state;

class encoding {
public:
	mpz_class cval;
	long degree;
	clt_state* key;

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

class clt_state {
public:
	gmp_randclass* 	rng;
	mpz_class* 		p; 				//	[N];
	mpz_class 		x0, z, zkappa;
	mpz_class 		zinv; 			//	[N];
	mpz_class* 		crtCoeff; 		//	[N];
	mpz_class* 		g; 				//	[N];
	mpz_class 		pzt;
	encoding 		y;

    unsigned long secparam;
    unsigned long n;
    unsigned long nzs;
    unsigned long rho;
    unsigned long nu;
    unsigned long kappa;
    unsigned long beta;

    clt_state
    ( 
        unsigned long secparam,
        unsigned long kappa,
        unsigned long nzs,
        unsigned long etap,
        int verbose = 0
    );

	~clt_state();

    encoding encode(mpz_class m);
    encoding encode(vector<mpz_class> m);

	mpz_class reduce(const mpz_class &c);
	unsigned long get_noise(const mpz_class& c, unsigned long degree);
	mpz_class zero_test(const mpz_class &c, unsigned long degree);
	unsigned long nbBits(const mpz_class &v);
	bool is_zero(const encoding &c);
	mpz_class& get_x0() { return x0; };
};

#endif 
// #ifndef __MULTIMAP_H
