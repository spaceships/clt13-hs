CLT13
=====

Haskell implementation of the [CLT13 multilinear map](https://eprint.iacr.org/2013/183), modified to be asymmetric (uses indices instead of levels).

It's pretty fast, but not as fast as the C one in [Alex Malozemoff's obfuscation implementation](https://github.com/amaloz/obfuscation).

This project exits to help me debug an obfuscation project. It isn't intended for much more than that.

Usage
=====

See [Test.hs](https://github.com/spaceships/clt13/blob/master/Test.hs) for example usage. 

Setup parameters:
* kappa: the maximum number of multiplications in a top-level encoding
* lambda: security parameter, used to set 50% of the other parameters
* nzs: the number of distinct indices in a toplevel encoding
* topLevelIndex: the actual index-set of a toplevel encoding, used to communicate the powers to the setup routine

License
=======

Licenced under WTFPLv2
