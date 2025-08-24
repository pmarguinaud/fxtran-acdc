# NAME

Fxtran::Generate

# DESCRIPTION

This module contains entry points for source code transformation and generation methods.

# METHODS

## semiimplicit

This is the method for transforming top-level semi-implicit routines such as `spcsi.F90` (hydrostatic)
and `spnhsi.F90` (non-hydrostatic).

The principle is to transform all zones with parallelism (`DO JSP`) into OpenACC or OpenMP kernels. Vertical operator
routines are supposed to be transformed with the singleblock method and called from `spcsi.F90` or `spnhsi.F90`.

Horizontal operators are supposed to be flagged with `!$ACDC HORIZONTAL` directives; the accelerated code for these
routines is supposed to be enabled by passing an extra argument `LDACC=.TRUE.` to the original horizontal routine.

## singlecolumn

This is the method used to transform a full routine into its single-column version, ready for accelerators:

This involves the following steps:

- Remove all loops on the `NPROMA` (aka `KLON` dimension).
- Set the iterator `JLON` (resp. `JROF`) to `KIDIA` (resp. `KST`).
- Allocate temporary arrays in a pre-allocated a stack (`YDSTACK`). These arrays are shared
by all threads belonging to the same warp.

# SEE ALSO

[fxtran-f90](fxtran-f90.md), [fxtran-gen](fxtran-gen.md)

# AUTHOR

philippe.marguinaud@meteo.fr
