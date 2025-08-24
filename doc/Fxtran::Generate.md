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
loop on the `NPROMA` dimension is transformed into an OpenACC kernel. The result of the 
transformation is a routine which runs on the CPU, but spawns several kernels on the device.

Arguments (`NPROMA` arrays and structure holding constant data such as `YDMODEL`) 
are supposed to be present on the device when the generated routine is called. 

See [Fxtran::SingleBlock](Fxtran%3A%3ASingleBlock.md) for more details.

## manyblocks

This method transform vector routines (processing a single `NPROMA` block) into routines 
processing several `NPROMA` blocks. The result of the transformation is a routine which 
executes on the CPU, but spawns OpenACC kernels. This method can be combined with the
singlecolumn method (ie OpenACC kernels can contain calls to singlecolumn routines).

Argument arrays and data structures containing constant data are supposed to be present 
on the device when the result routine is called.

Note that passing arrays with several blocks implies that routine dummy argument arrays
be declared with implicit shapes, as sometimes, actual arguments arrays are array sections,
and we cannot have the compiler creating array copies.

See [Fxtran::ManyBlocks](Fxtran%3A%3AManyBlocks.md) for more details.

## methods

This method creates method for handling data structures (FORTRAN derived types).

These methods are:

- SAVE

    Serialize data structure to a file. This is usefull for creating standalone 
    test cases.

- LOAD

    Read back a data structure from a file.

- CRC64

    Compute a checksum from a data structure; this is usefull to see whether a data
    structure has changed over time.

- COPY

    Create and copy the data structure contents to the device.

- WIPE

    Delete the data structure from the device.

See [Fxtran::IO](Fxtran%3A%3AIO.md) for more details.

## interface

This method create interface blocks for the original routine and the routines that can be obtained
by transforming this routine. 

For instance, if the routine to be transformed contains the following directives:

    SUBROUTINE SIGAM (...)

    !$ACDC singlecolumn
    !$ACDC singleblock

Then this method will create interfaces for `SIGAM`, `SIGAM_SINGLECOLUMN`, `SIGAM_SINGLEBLOCK`. 
These interfaces will be written to the same file if the option `merge-interfaces` is enabled.

See `Fxtran::Interface` for more details.

## bitrepro

This routine transforms the current routine into a routine where architecture dependant
intrinsics (`SIN`, `EXP`, etc.) are replaced by portable versions of these functions.

See `Fxtran::BitRepro` for more details.

## toplevel

This method transforms a routine several `NPROMA` blocks. Sections delimited by
`!$ACDC PARALLEL` directives are searched and call statements inside these
section are replaced by calls to parallel versions.

For instance:

    !$ACDC PARALLEL {

      CALL CPG (YDGEO, YLCPG_BNDS, YDCPG_OPTS, YDFORCESPPT, YLCPG_TND, YLCPG_SL1AUX, YDCPG_SL2, YLCPG_MISC, YDGPAR, &
        & YLCPG_PHY0, YLCPG_PHY9, YLMF_PHYS, YDHGRAD, YLCPG_DDH, YLCPG_DYN0, YLCPG_DYN9, YDMF_PHYS_SURF, YDVARS, &
        & YDGEOMVARS, YDXFU, YDCFU, YDMODEL, YDFIELDS, YDTRC, YDRADF, YDA_GFLSLP, YLA_SAVTEND, YDCPG_DDH_TND, YDPGTERM, &
        & YDA_GFLPC, YDA_GFLPT, YLA_ISETTLOFF, YDSLHD, YDCPG_SL1, YDA_EXTRA, YDDDH, YDTDDH, YDA_RSAVEDP, YDA_PWRL9, &
        & YDA_ZGEO0, YDA_ZRCP0, YDA_ZPRE0F, YDA_ZCTY0, LDWITH_MGRIDS, YDPHYSMWAVE)

    !$ACDC }

yields:

    CALL CPG_PARALLEL (YDGEO, YLCPG_BNDS, YDCPG_OPTS, YDFORCESPPT, YLCPG_TND, YLCPG_SL1AUX, YDCPG_SL2, YLCPG_MISC, YDGPAR, &
      & YLCPG_PHY0, YLCPG_PHY9, YLMF_PHYS, YDHGRAD, YLCPG_DDH, YLCPG_DYN0, YLCPG_DYN9, YDMF_PHYS_SURF, YDVARS, &
      & YDGEOMVARS, YDXFU, YDCFU, YDMODEL, YDFIELDS, YDTRC, YDRADF, YDA_GFLSLP, YLA_SAVTEND, YDCPG_DDH_TND, YDPGTERM, &
      & YDA_GFLPC, YDA_GFLPT, YLA_ISETTLOFF, YDSLHD, YDCPG_SL1, YDA_EXTRA, YDDDH, YDTDDH, YDA_RSAVEDP, YDA_PWRL9, &
      & YDA_ZGEO0, YDA_ZRCP0, YDA_ZPRE0F, YDA_ZCTY0, LDWITH_MGRIDS, YDPHYSMWAVE)

Moreover, constructs such as:

    !$ACDC COPY, IF=LLPARALLEL {
    
    LLPERSISTENT = LLPARALLEL
    
    CALL YLCPG_DYN0%INIT (0_JPIM, YDFIELDS%REGISTRY, NLEV=NFLEVG, PERSISTENT=LLPERSISTENT, &
                        & YDDYNA=YDMODEL%YRML_DYN%YRDYNA)
    
    CALL YLCPG_PHY0%INIT (0_JPIM, YDFIELDS%REGISTRY, NLEV=NFLEVG, LDMF_PHYS=LMPHYS.OR.LSIMPH, &
                        & PERSISTENT=LLPERSISTENT)
    
    ...
    
    !$ACDC }

and:

    !$ACDC WIPE, IF=LLPARALLEL {

    CALL YLCPG_TND%FINAL
    CALL YLCPG_DDH%FINAL

    ...

    !$ACDC }

are replaced by:

    LLPERSISTENT=LLPARALLEL
    CALL YLCPG_DYN0%INIT (0_JPIM, YDFIELDS%REGISTRY, NLEV=YDGEOMETRY%YRDIMV&
    &%NFLEVG, PERSISTENT=LLPERSISTENT, YDDYNA=YDMODEL%YRML_DYN%YRDYNA)
    
    IF (LLPARALLEL) THEN
      CALL ACDC_COPY (YLCPG_DYN0)
    ENDIF
    
    CALL YLCPG_PHY0%INIT (0_JPIM, YDFIELDS%REGISTRY, NLEV=YDGEOMETRY%YRDIMV%NFLEVG, LDMF_PHYS=YDMODEL&
    &%YRML_PHY_MF%YRPHY%LMPHYS.OR.YDMODEL%YRML_PHY_MF%YRSIMPHL%LSIMPH, PERSISTENT=LLPERSISTENT)
    
    IF (LLPARALLEL) THEN
      CALL ACDC_COPY (YLCPG_PHY0)
    ENDIF

and:

    IF (LLPARALLEL) THEN
      CALL ACDC_WIPE (YLCPG_TND)
    ENDIF

    CALL YLCPG_TND%FINAL

    IF (LLPARALLEL) THEN
      CALL ACDC_WIPE (YLCPG_DDH)
    ENDIF

    CALL YLCPG_DDH%FINAL

This allows for copying data structures after initalizing them, and removing them from
the device before deleting them.

Eventually, note that the result of the transformation is compiled **IN PLACE**
of the original routine.

See [Fxtran::TopLevel](Fxtran%3A%3ATopLevel.md) for more details.

# SEE ALSO

[fxtran-f90](fxtran-f90.md), [fxtran-gen](fxtran-gen.md)

# AUTHOR

philippe.marguinaud@meteo.fr
