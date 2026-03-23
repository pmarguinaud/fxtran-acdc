# NAME

[Fxtran::Intrinsic](../lib/Fxtran/Intrinsic.pm)

# DESCRIPTION

Handles Fortran intrinsic functions, in particular for bit-reproducibility
transformations.  Provides a predicate to test whether a name is a known
intrinsic, and functions that replace elementary math intrinsics and the `**`
operator with wrapper functions from the `FXTRAN_ACDC_BR_INTRINSICS` module,
ensuring portable, bit-reproducible results across different compilers and
platforms.  Inline include files (`*.func.h`) and post-CONTAINS include files
are also processed.

# FUNCTIONS
