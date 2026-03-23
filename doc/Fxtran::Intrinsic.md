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

## isIntrinsic

Returns a true value if the given name is a known Fortran intrinsic (including
bit-reproducibility candidates), false otherwise.

## slurp

Reads and returns the entire contents of a file as a single string.

## makeBitReproducibleSection

Replaces intrinsic function calls and `**` operators within a single AST
subtree with their `FXTRAN_ACDC_BR_*` bit-reproducible equivalents.
Returns the number of replacements made.

## makeBitReproducible

Applies bit-reproducibility substitutions to an entire program unit, including
inline `*.func.h` include files and post-CONTAINS include files.  Adds the
required `USE FXTRAN_ACDC_BR_INTRINSICS` statement when substitutions are made.
