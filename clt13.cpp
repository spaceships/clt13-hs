#include "clt13.hpp"

using std::cout;
using std::endl;

// helpers/*{{{*/

// Return current time in sec.
double currentTime() {
    timeval t;
    gettimeofday(&t,NULL);
    return (double) (t.tv_sec+(double)(t.tv_usec/1000000.0));
}

// Generate a (centered) random of nbBits using rng
inline mpz_class generateRandom(unsigned long nbBits, gmp_randclass* rng) {
    if (nbBits <= 1) return rand()%2;
    else return rng->get_z_bits(nbBits)-(mpz_class(1)<<(nbBits-1));
}

// Compute a mod b, a>=0
inline mpz_class mod(const mpz_class &a, const mpz_class &b) {
    mpz_class res;
    mpz_mod (res.get_mpz_t(), a.get_mpz_t(), b.get_mpz_t());
    return res;
}

// Compute a mod b, -b/2 < a <= b/2
inline mpz_class modNear(const mpz_class &a, const mpz_class &b) {
    mpz_class res;
    mpz_mod (res.get_mpz_t(), a.get_mpz_t(), b.get_mpz_t());
    if (res > (b>>1)) res -= b;
    return res;
}

// Compute nearest value of a/b
inline mpz_class quotNear(const mpz_class &a, const mpz_class &b) {
    return (a-modNear(a,b))/b;
}

// Initialization of a ciphertext to 0
encoding::encoding() {
    cval = 0;
    degree = 0;
}
/*}}}*/
// index tools/*{{{*/

index_set index_union (const index_set &x, const index_set &y) {
    index_set ret;
    set_union (x.begin(), x.end(), y.begin(), y.end(), inserter(ret, ret.begin()));
    return ret;
}


bool distinct_indices (const index_set &x, const index_set &y) {
    index_set ret;
    set_intersection(x.begin(), x.end(), y.begin(), y.end(), inserter(ret, ret.begin()));
    return ret.size() == 0;
}


vector<index_set> exclusive_partition_family (unsigned lambda, unsigned d, unsigned i, unsigned start) { 
    unsigned a[d];
    unsigned b[d][lambda];
    unsigned x = start;
    for (unsigned j = 0; j < d; j++) {
        a[j] = x++;
    }
    for (unsigned j = 1; j < d; j++) {
        for (unsigned k = 0; k < lambda; k++) {
            b[j][k] = x++;
        }
    }
    vector< index_set > res;
    res.push_back({ start });
    for (unsigned j = 1; j < d; j++) {
        for (unsigned k = 0; k < lambda; k++) {
            if (i & (1 << k)) {
                res[0].insert(b[j][k]);
            }
        }
    }
    for (unsigned j = 1; j < d; j++) {
        res.push_back({ a[j] });
        for (unsigned k = 0; k < lambda; k++) {
            if (!(i & (1 << k))) {
                res[j].insert( b[j][k] );
            }
        }
    }
    return res;
}
/*}}}*/
////////////////////////////////////////////////////////////////////////////////
// ciphertext class
encoding::encoding(const encoding& c) {/*{{{*/
    key = c.key;
    cval = c.cval;
    degree = c.degree;
}
/*}}}*/
encoding::encoding(clt_state* mmkey, mpz_class c, unsigned long deg) {/*{{{*/
    key = mmkey;
    cval = c;
    degree = deg;
}
/*}}}*/
unsigned long encoding::get_noise() {/*{{{*/
    return key->get_noise(cval, degree);
}
/*}}}*/
encoding& encoding::operator=(const encoding& c) {/*{{{*/
  key = c.key;
  cval = c.cval;
  degree = c.degree;

  return *this;
}
/*}}}*/
encoding& encoding::operator+=(const encoding& c) {/*{{{*/
    assert(degree == c.degree);
    cval += c.cval;
    cval = key->reduce(cval);
    return *this;
}
/*}}}*/
encoding& encoding::operator-=(const encoding& c) {/*{{{*/
    assert(degree == c.degree);
    cval -= c.cval;
    cval = key->reduce(cval);
    return *this;
}
/*}}}*/
encoding& encoding::operator*=(const encoding& c) {/*{{{*/
    if (degree+c.degree>0)
        degree += c.degree;
    cval *= c.cval;
    cval = key->reduce(cval);
    return *this;
}
/*}}}*/
////////////////////////////////////////////////////////////////////////////////
// clt_state class
clt_state::clt_state/*{{{*/
( 
    unsigned long secparam,
    unsigned long kappa_,
    unsigned long nzs_,
    unsigned long etap,
    int verbose
){
    kappa = kappa_;
    /* Calculate CLT parameters */
    unsigned long alpha, eta, rho_f;
    alpha = secparam;
    beta  = secparam;
    rho   = secparam;
    rho_f = kappa * (rho + alpha + 2);
    eta   = rho_f + alpha + 2 * beta + secparam + 8;
    nu    = eta - beta - rho_f - secparam + 3; // threshold for zero testing
    n     = (int) (eta * log2((float) secparam));
    nzs   = nzs_;

    unsigned long i, j;
    double startTime;

    // Define PRNG
	rng = new gmp_randclass(gmp_randinit_default);

    if (eta % etap) {
        etap += eta % etap;
    }
    
    if (verbose) {
        fprintf(stderr, "  Security Parameter: %ld\n", secparam);
        fprintf(stderr, "  Kappa: %ld\n", kappa);
        fprintf(stderr, "  Alpha: %ld\n", alpha);
        fprintf(stderr, "  Beta: %ld\n", beta);
        fprintf(stderr, "  Eta: %ld\n", eta);
        fprintf(stderr, "  Etap: %ld\n", etap);
        fprintf(stderr, "  Nu: %ld\n", nu);
        fprintf(stderr, "  Rho: %ld\n", rho);
        fprintf(stderr, "  Rho_f: %ld\n", rho_f);
        fprintf(stderr, "  N: %ld\n", n);
        fprintf(stderr, "  Number of Zs: %ld\n", nzs);
    }

    // Generate the p_i's
    // /!\ Generating primes of eta bits can be very long, 
    // so eventually we generate p as product of primes of etap bits
    x0 = 1;
    unsigned long niter=(unsigned)eta/etap;
    unsigned long psize;

    startTime=currentTime();
    std::cout << "Generate the p_i's and x0: " << std::flush;
    mpz_class p_tmp, p_unif;
    p = new mpz_class[n];
    #pragma omp parallel for private(p_tmp, p_unif, j)
    for (i=0; i<n; i++)
    {
        p[i] = 1;
        for (j=0; j<niter; j++)
        {
          if (j<(niter-1)) psize=etap;
          else psize=eta-etap*(niter-1);
          
          p_unif = rng->get_z_bits(psize);
          
          mpz_nextprime(p_tmp.get_mpz_t(), p_unif.get_mpz_t());
          p[i] *= p_tmp;
        }
        #pragma omp critical
        {
            x0 *= p[i];
        }
    }
    std::cout << (double)(currentTime()-startTime) << "s" << std::endl;

    // Generate the CRT Coefficients
    // /!\ This requires a lot of RAM (>100 GB for extra instantiation)
    startTime=currentTime();
    std::cout << "Generate the crtCoeff_i's: " << std::flush;
    mpz_class Q;
    crtCoeff = new mpz_class[n];
    #pragma omp parallel for private(Q)
    for (i=0; i<n; i++)
    {
        Q = x0/p[i];
        mpz_invert(crtCoeff[i].get_mpz_t(), Q.get_mpz_t(), p[i].get_mpz_t());
        crtCoeff[i] *= Q;
    }
    std::cout << (double)(currentTime()-startTime) << "s" << std::endl;

    // Generate the g_i's
    startTime=currentTime();
    std::cout << "Generate the g_i's: " << std::flush;
    mpz_class g_tmp;
    g = new mpz_class[n];
#pragma omp parallel for private(g_tmp)
    for (i=0; i<n; i++)
    {
        g_tmp = rng->get_z_bits(alpha);
        mpz_nextprime(g[i].get_mpz_t(), g_tmp.get_mpz_t());
    }
    std::cout << (double)(currentTime()-startTime) << "s" << std::endl;

    // Generate z
    startTime=currentTime();
    std::cout << "Generate z and zinv: " << std::flush;
    int ret;
    do {
        z = rng->get_z_range(x0);
        ret = mpz_invert(zinv.get_mpz_t(), z.get_mpz_t(), x0.get_mpz_t());
    } while (ret == 0);
    std::cout << (double)(currentTime()-startTime) << "s" << std::endl;

    // Generate y
	std::cout << "Generate y: " << std::flush;
	startTime=currentTime();
	y = encode((unsigned long) 1);
	std::cout << (double)(currentTime()-startTime) << "s" << std::endl;
    
    // Generate zero-tester pzt
    std::cout << "Generate the zero-tester pzt: " << std::flush;
    startTime=currentTime();
    mpz_class input;
    zkappa=1;
    for (i=0; i<kappa; i++)
        zkappa = mod(zkappa*z, x0);
    pzt=0;
#pragma omp parallel for private(i, input)
    for (i=0; i<n; i++) {
        mpz_invert(input.get_mpz_t(), g[i].get_mpz_t(), p[i].get_mpz_t());
        input = mod(input*zkappa, p[i])*generateRandom(beta, rng)*(x0/p[i]); 
#pragma omp critical
        {
            pzt += input;
        }
    }
    pzt = mod(pzt,x0);
    std::cout << (double)(currentTime()-startTime) << "s" << std::endl;
}

//// Encrypt bit vector b using public key (subset sum)
//encoding clt_state::Encrypt(bool b[ell]) {
    //mpz_class c=0;
    //for (long i=0; i<ell; i++)
        //if (b[i]) c += xp[i];
    //return encoding(this, mod(c, x0), 0);
//}

encoding clt_state::Encrypt(unsigned long m) {
    mpz_class cs[n];
    mpz_class mp = m;

    for (unsigned long i=0; i<n; i++)
        mpz_mod(cs[i].get_mpz_t(), mp.get_mpz_t(), g[i].get_mpz_t());

    mpz_class c = Encrypt_with_sk(cs, rho, 1);
    return encoding(this, c, 1);
}

// Encrypt input ARRAY `m' with `nbBits' random and initial degree `degree' with secret key
mpz_class clt_state::Encrypt_with_sk(mpz_class* m, unsigned long nbBits, unsigned long degree) {
    std::cout << ". " << std::flush;

    unsigned long i, j;

    mpz_class res=0;

    for (i=0; i<n; i++) {
        res += (m[i] + g[i]*generateRandom(nbBits, rng)) * crtCoeff[i];
    }

    res = mod(res, x0);
    for (j=degree; j>0; j--)
        res = mod(res*zinv, x0);

    std::cout << "* " << std::flush;

    return res;
}

// Destruction of clt_state
clt_state::~clt_state() {
    //
}

// Encrypt input VALUE `m' with `nbBits' random and initial degree `degree' with secret key
mpz_class clt_state::Encrypt_with_sk(unsigned long m, unsigned long nbBits, unsigned long degree) {
    std::cout << ". " << std::flush;

    unsigned long i, j;

    mpz_class input, res=0;
    for (i=0; i<n; i++) {
        if (m <= 1)
            input = m + g[i]*generateRandom(nbBits, rng);
        else
            input = generateRandom(m, rng) + g[i]*generateRandom(nbBits, rng);

        res += input*crtCoeff[i];
    }

    res = mod(res, x0);
    for (j=degree; j>0; j--)
        res = mod(res*zinv, x0);

    std::cout << "* " << std::flush;

    return res;
}

// Compute c mod x0
mpz_class clt_state::reduce(const mpz_class &c) {
    return mod(c, x0);
}

// Get noise in a ciphertext value c of degree `degree'
unsigned long clt_state::get_noise(const mpz_class& c, unsigned long degree) {
    unsigned long i;
    mpz_class value = c;
    unsigned long max = 0, nbBits;
    mpz_class noise;

    for (i=degree; i>0; i--) {
        value *= z;
    }

#pragma omp parallel for private(noise, nbBits)
    for (i=0; i<n; i++) {
        noise = quotNear(modNear(value, p[i]), g[i]);
        nbBits = mpz_sizeinbase(noise.get_mpz_t(),2);
#pragma omp critical
        {
            if (nbBits>max) max = nbBits;
        }
    }
    return max;
}

// Return w = (c*y^(kappa-degree))*pzt mod x0
// We multiply by y^(kappa-degree) to transform the degree-level
// encoding into a kappa-level encoding, so that the z^kappa mask
// included in pzt can be canceled out
mpz_class clt_state::zero_test(const mpz_class &c, unsigned long degree) {
    assert(degree<=kappa);

    mpz_class value = c;

    for (unsigned long i=kappa-degree; i>0; i--) {
        value = modNear(value*y.cval, x0);
    }
    value = modNear(value*pzt,x0);

    return value;
}

// Return number of bits of pzt
unsigned long clt_state::nbBits(const mpz_class &pzt) {
    return mpz_sizeinbase(pzt.get_mpz_t(), 2);
}

// Check whether c is an encoding of 0
bool clt_state::is_zero(const encoding &c) {
    mpz_class value = zero_test(c.get_cval(), c.get_degree());
    return (nbBits(value)<(nbBits(x0)-nu)) ? 1 : 0;
}
