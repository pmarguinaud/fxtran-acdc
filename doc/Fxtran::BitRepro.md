# NAME

[Fxtran::BitRepro](../lib/Fxtran/BitRepro.pm)

# DESCRIPTION

The purpose of this module is to provide functions to make the code bit-reproducible
when results are compared between the CPU and the GPU; this involves two steps:

- Replacing transcendental intrinsics with portable functions.
- Adding brackets in additions to force the compiler to add
number in a well defined order.

The generated code has to be compiled with the `-O0` option, so that results 
be reproduced between CPU and GPU.

# SEE ALSO

[fxtran\_acdc\_br\_intrinsics.F90](../src/fxtran_acdc_br_intrinsics.F90),
[fxtran\_acdc\_br\_transcendentals.cc](../src/fxtran_acdc_br_transcendentals.cc),
[Fxtran::Intrinsic](Fxtran%3A%3AIntrinsic.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

## makeBitReproducible

Entry point for the bit-reproducibility transformation. Iterates over all
program units in the document and dispatches to `processSingleModule` or
`processSingleRoutine` depending on the kind of program unit.

## processSingleModule

Apply the bit-reproducibility transformation to every program unit contained
in a module, and then rename the module by appending the bit-repro suffix.

## traverseAdditionSubstractionExpr

Recursively decompose an addition/subtraction expression into a flat list of
operand and operator nodes. Returns the expression unchanged if it is not a
two-operand addition or subtraction.

## addBitReproParens

Insert explicit parentheses around addition/subtraction chains in a statement
so that the evaluation order is fixed and results are reproducible across
different compiler optimisation levels.

## processSingleRoutine

Apply the full bit-reproducibility transformation to a single subroutine: replace
transcendental intrinsics, optionally bracket additions, rename call targets and
the subroutine itself with the bit-repro suffix, then recurse into contained
program units.
