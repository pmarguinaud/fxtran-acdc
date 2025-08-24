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

See [Fxtran::SemiImplicit](Fxtran%3A%3ASemiImplicit.md) for more details.

## singlecolumn

This is the method used to transform a full vector routine (ie processing a full `NPROMA` block) 
into its single-column version, ready for accelerators:

This involves the following steps:

- Remove all loops on the `NPROMA` (aka `KLON` dimension).
- Set the iterator `JLON` (resp. `JROF`) to `KIDIA` (resp. `KST`).
- Allocate temporary arrays in a pre-allocated a stack (`YDSTACK`). These arrays are shared
by all threads belonging to the same warp.
- Insert an OpenACC `!$acc routine seq`) directive.

See [Fxtran::SingleColumn](Fxtran%3A%3ASingleColumn.md) for more details.

## pointerparallel

This method transforms a vector routine (processing a single `NPROMA` block) into a parallel routines, that is,
a routine containing many OpenMP/OpenACC kernels.

- `!$ACDC PARALLEL` sections are searched.
- Each section, depending on the user-provided options, may be transformed into the one or many of the following 
kernels: OpenMP, OpenMPSingleColumn, OpenACCSingleColumn

    The selection of the kernel variant is chosen at run time.

- Some instrumentation may be added: measurement of GPU memory, synchronisation of data on the host 
after each kernel.

See [Fxtran::Pointer::Parallel](Fxtran%3A%3APointer%3A%3AParallel.md) for more details.

## singleblock

This transforms a vector routine (processing a single `NPROMA` block) into a routine where each
loop on the `NPROMA` dimension is transformed into an OpenACC kernel.

Arguments (`NPROMA` arrays and structure holding constant data such as `YDMODEL`) 
are supposed to be present on the device when the generated routine is called. 

See [Fxtran::SingleBlock](Fxtran%3A%3ASingleBlock.md) for more details.

# SEE ALSO

[fxtran-f90](fxtran-f90.md), [fxtran-gen](fxtran-gen.md)

# AUTHOR

philippe.marguinaud@meteo.fr
