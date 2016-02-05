#include <iostream>
#include "clt_mlm.h"

using namespace std;
typedef unsigned long ulong;

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
    srand(time(NULL));

    ulong nzs     = 10;
    ulong lambda  = 30;
    ulong kappa   = 2;
    ulong verbose = true;

    clt_mlm_state mmap;
    mmap.secparam = lambda;
    mmap.nzs      = nzs;

    int pows [nzs];
    for (ulong i = 0; i < nzs; i++) pows[i] = 1;

    //clt_mlm_setup(struct clt_mlm_state *s, const char *dir, const long *pows,
    //              long kappa, long size, int verbose)
    clt_mlm_setup(&mmap, NULL, pows, kappa, 0, verbose);

    mpz_t x [1];
    mpz_init_set_ui(x[0], 0);
    while (mpz_cmp_ui(x[0], 0) <= 0) {
        mpz_set_ui(x[0], rand());
        mpz_mod(x[0], x[0], mmap.gs[0]);
    }
    gmp_printf("x = %Zd\n", x[0]);

    mpz_t zero [1];
    mpz_init_set_ui(zero[0], 0);

    mpz_t one [1];
    mpz_init_set_ui(one[0], 1);

    //void
    //clt_mlm_encode(struct clt_mlm_state *s, mpz_t out, size_t nins,
                   //const mpz_t *ins, unsigned int nzs, const int *indices,
                   //const int *pows)

    // zero by zero addition test
    int ix [nzs];
    for (ulong i = 0; i < nzs; i++) {
        ix[i] = i;
    }
    mpz_t x0, x1, xp;
    mpz_inits(x0, x1, xp, NULL);
    clt_mlm_encode(&mmap, x0, 1, zero, nzs, ix, pows);
    clt_mlm_encode(&mmap, x1, 1, zero, nzs, ix, pows);
    mpz_add(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);
    //int
    //clt_mlm_is_zero(const mpz_t c, const mpz_t pzt, const mpz_t q, long nu)
    bool ok = expect("is_zero(0 + 0) = ", 1, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));

    clt_mlm_encode(&mmap, x0, 1, zero, nzs, ix, pows);
    clt_mlm_encode(&mmap, x1, 1, one,  nzs, ix, pows);
    mpz_add(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);
    ok &= expect("is_zero(0 + 1) = ", 0, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));

    clt_mlm_encode(&mmap, x0, 1, zero, nzs, ix, pows);
    clt_mlm_encode(&mmap, x1, 1, x,  nzs, ix, pows);
    mpz_add(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);
    ok &= expect("is_zero(0 + x) = ", 0, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));

    clt_mlm_encode(&mmap, x0, 1, x, nzs, ix, pows);
    clt_mlm_encode(&mmap, x1, 1, x, nzs, ix, pows);
    mpz_sub(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);
    ok &= expect("is_zero(x - x) = ", 1, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));

    clt_mlm_encode(&mmap, x0, 1, zero, nzs, ix, pows);
    clt_mlm_encode(&mmap, x1, 1, x,    nzs, ix, pows);
    mpz_sub(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);
    ok &= expect("is_zero(0 - x) = ", 0, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));

    clt_mlm_encode(&mmap, x0, 1, one,  nzs, ix, pows);
    clt_mlm_encode(&mmap, x1, 1, zero, nzs, ix, pows);
    mpz_sub(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);
    ok &= expect("is_zero(1 - 0) = ", 0, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));

    int ix0 [nzs];
    int ix1 [nzs];
    for (ulong i = 0; i < nzs; i++) {
        if (i < nzs / 2) {
            ix0[i] = i;
            ix1[i] = -1;
        } else {
            ix0[i] = -1;
            ix1[i] = i;
        }
    }
    clt_mlm_encode(&mmap, x0, 1, x   , nzs, ix0, pows);
    clt_mlm_encode(&mmap, x1, 1, zero, nzs, ix1, pows);
    mpz_mul(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);
    ok &= expect("is_zero(x * 0) = ", 1, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));

    clt_mlm_encode(&mmap, x0, 1, x  , nzs, ix0, pows);
    clt_mlm_encode(&mmap, x1, 1, one, nzs, ix1, pows);
    mpz_mul(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);
    ok &= expect("is_zero(x * 1) = ", 0, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));

    clt_mlm_encode(&mmap, x0, 1, x, nzs, ix0, pows);
    clt_mlm_encode(&mmap, x1, 1, x, nzs, ix1, pows);
    mpz_mul(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);
    ok &= expect("is_zero(x * x) = ", 0, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));

    // zimmerman-like test

    mpz_t c;
    mpz_t in0 [2];
    mpz_t in1 [2];
    mpz_t cin [2];

    mpz_inits(c, in0[0], in0[1], in1[0], in1[1], cin[0], cin[1], NULL);

    mpz_urandomb(in1[0], mmap.rng, mmap.secparam);
    mpz_mod(in1[0], in1[0], mmap.gs[0]);

    mpz_set_ui(in0[0], 0);
    mpz_set_ui(cin[0], 0);

    mpz_urandomb(in0[1], mmap.rng, 16);
    mpz_urandomb(in1[1], mmap.rng, 16);
    mpz_mul(cin[1], in0[1], in1[1]);

    clt_mlm_encode(&mmap, x0, 2, in0, nzs, ix0, pows);
    clt_mlm_encode(&mmap, x1, 2, in1, nzs, ix1, pows);
    clt_mlm_encode(&mmap, c,  2, cin, nzs, ix,  pows);

    mpz_mul(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);

    mpz_sub(xp, xp, c);
    mpz_mod(xp, xp, mmap.q);

    ok &= expect("zimmerman test is_zero(0 * x) = ", 1, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));

    mpz_set_ui(in0[0], 1);
    mpz_set_ui(in1[0], 1);
    mpz_set_ui(cin[0], 0);

    mpz_urandomb(in0[0], mmap.rng, mmap.secparam);
    mpz_mod(in0[0], in0[0], mmap.gs[0]);

    mpz_urandomb(in1[0], mmap.rng, mmap.secparam);
    mpz_mod(in1[0], in1[0], mmap.gs[0]);

    mpz_urandomb(in0[1], mmap.rng, 16);
    mpz_urandomb(in1[1], mmap.rng, 16);
    mpz_mul(cin[1], in0[1], in1[1]);

    clt_mlm_encode(&mmap, x0, 2, in0, nzs, ix0, pows);
    clt_mlm_encode(&mmap, x1, 2, in1, nzs, ix1, pows);
    clt_mlm_encode(&mmap, c,  2, cin, nzs, ix,  pows);

    mpz_mul(xp, x0, x1);
    mpz_mod(xp, xp, mmap.q);

    mpz_sub(xp, xp, c);
    mpz_mod(xp, xp, mmap.q);

    ok &= expect("zimmerman test is_zero(x * y) = ", 0, clt_mlm_is_zero(xp, mmap.pzt, mmap.q, mmap.nu));
    return !ok;
}
