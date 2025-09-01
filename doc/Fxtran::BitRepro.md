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
[fxtran\_acdc\_br\_transcendentals.cc](../src/fxtran_acdc_br_transcendentals.cc)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
