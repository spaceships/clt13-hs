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
    mmap  = c.mmap;
    value = c.value;
    index = c.index;
}
/*}}}*/
encoding::encoding(clt_state* mmkey, mpz_class c, const index_set &ix) {/*{{{*/
    mmap  = mmkey;
    value = c;
    index = ix;
}
/*}}}*/
encoding& encoding::operator=(const encoding& c) {/*{{{*/
  mmap  = c.mmap;
  value = c.value;
  index = c.index;

  return *this;
}
/*}}}*/
encoding& encoding::operator+=(const encoding& c) {/*{{{*/
    assert(index == c.index);
    value += c.value;
    value = mmap->reduce(value);
    return *this;
}
/*}}}*/
encoding& encoding::operator-=(const encoding& c) {/*{{{*/
    assert(index == c.index);
    value -= c.value;
    value = mmap->reduce(value);
    return *this;
}
/*}}}*/
encoding& encoding::operator*=(const encoding& c) {/*{{{*/
    assert(distinct_indices(index, c.index));
    value *= c.value;
    value = mmap->reduce(value);
    index = index_union(index, c.index);
    return *this;
}
/*}}}*/

////////////////////////////////////////////////////////////////////////////////
// clt_state class
clt_state::clt_state
( 
    unsigned long secparam,
    unsigned long kappa_,
    unsigned long nzs_,
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
    nu    = eta - beta - rho_f - secparam - 3; // threshold for zero testing
    n     = (int) (eta * log2((float) secparam));
    nzs   = nzs_;

    unsigned long i;
    double startTime;

    mpz_class zs [nzs];
    zinvs = new mpz_class[nzs];

    // Define PRNG
	rng = new gmp_randclass(gmp_randinit_default);

    if (verbose) {
        fprintf(stderr, "  Security Parameter: %ld\n", secparam);
        fprintf(stderr, "  Kappa: %ld\n", kappa);
        fprintf(stderr, "  Alpha: %ld\n", alpha);
        fprintf(stderr, "  Beta: %ld\n", beta);
        fprintf(stderr, "  Eta: %ld\n", eta);
        fprintf(stderr, "  Nu: %ld\n", nu);
        fprintf(stderr, "  Rho: %ld\n", rho);
        fprintf(stderr, "  Rho_f: %ld\n", rho_f);
        fprintf(stderr, "  N: %ld\n", n);
        fprintf(stderr, "  Number of Zs: %ld\n", nzs);
    }

    x0 = 1;

    startTime=currentTime();
    std::cout << "Generate the p_i's and x0: " << std::flush;
    mpz_class p_tmp, p_unif;
    p = new mpz_class[n];
    #pragma omp parallel for private(p_tmp, p_unif)
    for (i=0; i<n; i++)
    {
        mpz_class p_unif = rng->get_z_bits(eta);
        mpz_nextprime(p[i].get_mpz_t(), p_unif.get_mpz_t());
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
    std::cout << "Generate zs and zinvs: " << std::flush;
#pragma omp parallel for private(g_tmp)
    for (i = 0; i < nzs; i++) {
        int ret;
        // ensure the zs are invertible & invert them
        do {
            zs[i] = rng->get_z_range(x0);
            ret   = mpz_invert(zinvs[i].get_mpz_t(), zs[i].get_mpz_t(), x0.get_mpz_t());
        } while (ret == 0);
    }
    std::cout << (double)(currentTime()-startTime) << "s" << std::endl;

    // Generate zero-tester pzt
    std::cout << "Generate the zero-tester pzt: " << std::flush;
    startTime=currentTime();
    mpz_class input;
    zkappa = 1;
    for (i = 0; i < nzs; i++) {
        zkappa = mod(zkappa * zs[i], x0);
    }
    pzt=0;
#pragma omp parallel for private(i, input)
    for (i = 0; i < n; i++) {
        mpz_invert(input.get_mpz_t(), g[i].get_mpz_t(), p[i].get_mpz_t());
        input = mod(input * zkappa, p[i]) * generateRandom(beta, rng) * (x0 / p[i]); 
#pragma omp critical
        {
            pzt += input;
        }
    }
    pzt = mod(pzt,x0);
    std::cout << (double)(currentTime()-startTime) << "s" << std::endl;
}

encoding clt_state::encode(mpz_class m, const index_set &ix) {
    mpz_class cs[n];
    cs[0] = m;
    for (unsigned long i=1; i<n; i++)
        cs[i] = 0;
    mpz_class c = encrypt(cs, ix);
    return encoding(this, c, ix);
}

encoding clt_state::encode(vector<mpz_class> m, const index_set &ix) {
    mpz_class cs[n];
    for (unsigned long i=0; i < n; i++) {
        if (i < m.size()) {
            cs[i] = m[i];
        } else {
            cs[i] = 0;
        }
    }
    mpz_class c = encrypt(cs, ix);
    return encoding(this, c, ix);
}

// Encrypt input ARRAY `m' with `nbBits' random and initial degree `degree' with secret key
mpz_class clt_state::encrypt(mpz_class* m, const index_set &ix) {
    std::cout << ". " << std::flush;
    unsigned long i;
    mpz_class res=0;
    for (i = 0; i < n; i++) {
        res += (m[i] + g[i]*generateRandom(rho, rng)) * crtCoeff[i];
    }
    res = mod(res, x0);
    for (auto j: ix) {
        res = mod(res * zinvs[j], x0);
    }
    std::cout << "* " << std::flush;
    return res;
}

// Destruction of clt_state
clt_state::~clt_state() {
    //
}

// Compute c mod x0
mpz_class clt_state::reduce(const mpz_class &c) {
    return mod(c, x0);
}

index_set clt_state::top_level_index() {
    index_set ret;
    for (unsigned i = 0; i < nzs; i++) {
        ret.insert(i);
    }
    return ret;
}

// Compute a mod b, -b/2 < a <= b/2
inline mpz_class modNear(const mpz_class &a, const mpz_class &b) {
    mpz_class res;
    mpz_mod (res.get_mpz_t(), a.get_mpz_t(), b.get_mpz_t());
    if (res > (b >> 1)) 
        res -= b;
    return res;
}

bool clt_state::is_zero(const encoding &c) {
    assert(c.index == top_level_index());
    mpz_class tmp = modNear(c.value * pzt, x0);
    return mpz_sizeinbase(tmp.get_mpz_t(), 2) < (mpz_sizeinbase(x0.get_mpz_t(), 2) - nu);
}
