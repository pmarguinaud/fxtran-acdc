# NAME

[Fxtran::Generate](../lib/Fxtran/Generate.pm)

# DESCRIPTION

This module contains entry points for source code transformation and generation methods.

# METHODS

## semiimplicit

```
* semiimplicit
  --checker                      (FLAG) :                      : Sanity checks, produce a report
  --create-interface             (FLAG) :                      : Generate an interface file
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --dummy                        (FLAG) :                      : Generate a dummy routine (strip all executable code)
  --inline-comment               (FLAG) :                      : Add a comment when inlining a routine
  --inline-contained             (FLAG) :                      : Inline contained routines
  --inlined                      (LIST) : NONE                 : List of routines to inline
  --keep-drhook                  (FLAG) :                      : Keep DrHook
  --max-statements-per-parallel         : NONE                 : Maximum number of statements per parallel section
  --merge-interfaces             (FLAG) :                      : Consider that single column interfaces and regular interfaces are in the same include file
  --only-if-newer                (FLAG) :                      : Do not update file if unchanged content
  --openmptoparallel             (FLAG) :                      : Transform OpenMP parallel sections into ACDC parallel sections
  --parallel-iterator-list       (LIST) : NONE                 : List of iterators for generating parallel sections (add to JLON, JLEV) 
  --pragma                              : OpenACC              : Pragma (OpenACC or OpenMP)
  --redim-arguments              (FLAG) :                      : Transform 1D array arguments to scalars
  --set-variables                       : NONE                 : Apply variables values and simplify the code
  --stack-method                 (FLAG) :                      : Use stack method instead of macros
  --stack84                      (FLAG) :                      : Use separate stacks for data types of sizes 4 and 8
  --style                               : NONE                 : Source code style (default: guess from file contents)
  --suffix-semiimplicit                 : _SINGLEBLOCK         : Suffix for semi-implicit  routines
  --tmp                                 : .                    : Temporary directory for processing
  --value-attribute              (FLAG) :                      : Add VALUE attribute to scalar intrinsic arguments
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


This is the method for transforming top-level semi-implicit routines such as `spcsi.F90` (hydrostatic)
and `spnhsi.F90` (non-hydrostatic).

The principle is to transform all zones with parallelism (`DO JSP`) into OpenACC or OpenMP kernels. Vertical operator
routines are supposed to be transformed with the singleblock method and called from `spcsi.F90` or `spnhsi.F90`.

Horizontal operators are supposed to be flagged with `!$ACDC HORIZONTAL` directives; the accelerated code for these
routines is supposed to be enabled by passing an extra argument `LDACC=.TRUE.` to the original horizontal routine.

See [Fxtran::SemiImplicit](Fxtran%3A%3ASemiImplicit.md) for more details.

## singlecolumn

```
* singlecolumn
  --array-slice-to-address       (FLAG) :                      : Pass addresses of first array element instead of array slices
  --checker                      (FLAG) :                      : Sanity checks, produce a report
  --create-interface             (FLAG) :                      : Generate an interface file
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --dummy                        (FLAG) :                      : Generate a dummy routine (strip all executable code)
  --inline-comment               (FLAG) :                      : Add a comment when inlining a routine
  --inline-contained             (FLAG) :                      : Inline contained routines
  --inlined                      (LIST) : NONE                 : List of routines to inline
  --keep-drhook                  (FLAG) :                      : Keep DrHook
  --merge-interfaces             (FLAG) :                      : Consider that single column interfaces and regular interfaces are in the same include file
  --no-check-pointers-dims       (LIST) : NONE                 : List of pointer variables that should not be checked for their dimensions
  --only-if-newer                (FLAG) :                      : Do not update file if unchanged content
  --pragma                              : OpenACC              : Pragma (OpenACC or OpenMP)
  --process-interfaces           (FLAG) :                      : Transform interfaces into single column interfaces (used for MODI MESONH files)
  --process-pointers             (FLAG) :                      : Process pointers (change them to CRAY pointers)
  --redim-arguments              (FLAG) :                      : Transform 1D array arguments to scalars
  --set-variables                       : NONE                 : Apply variables values and simplify the code
  --stack-method                 (FLAG) :                      : Use stack method instead of macros
  --stack84                      (FLAG) :                      : Use separate stacks for data types of sizes 4 and 8
  --style                               : NONE                 : Source code style (default: guess from file contents)
  --suffix-singlecolumn                 : _OPENACC             : Suffix for generated routines
  --suffix-singlecolumn-called          : NONE                 : Suffix for singlecolumn routines called by routine being processed
  --tmp                                 : .                    : Temporary directory for processing
  --use-bit-repro-intrinsics     (FLAG) :                      : Use bit reproducible intrinsics
  --value-attribute              (FLAG) :                      : Add VALUE attribute to scalar intrinsic arguments
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


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

```
* pointerparallel
  --base                         (FLAG) :                      : Base directory for file lookup
  --checker                      (FLAG) :                      : Sanity checks, produce a report
  --contiguous-pointers          (FLAG) :                      : Add CONTIGUOUS attribute to pointer accessors
  --create-interface             (FLAG) :                      : Generate an interface file
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --files                        (LIST) : NONE                 : List of files to be looked at for inlining
  --gpumemstat                   (FLAG) :                      : Add calls to GPUMEMSTAT
  --inline-contained             (FLAG) :                      : Inline CONTAINed routines
  --inlined                      (LIST) : NONE                 : List of routines to inline
  --max-statements-per-parallel         : NONE                 : Maximum number of statements per parallel section
  --merge-interfaces             (FLAG) :                      : Consider that single column interfaces and regular interfaces are in the same include file
  --method-prefix                       : ACDC_                : Prefix for method names
  --only-if-newer                (FLAG) :                      : Do not update file if unchanged content
  --parallel-iterator-list       (LIST) : NONE                 : List of iterators for generating parallel sections (add to JLON, JLEV) 
  --parallelmethod-section       (FLAG) :                      : Embed parallelmethod information in binary
  --post-parallel                (LIST) : nullify              : Generate code after parallel section
  --pragma                              : OpenACC              : Pragma (OpenACC or OpenMP)
  --redim-arguments              (FLAG) :                      : Transform 1D array arguments to scalars
  --skip-arrays                  (LIST) : PGFL,PGFLT1,PGMVT1,PGPSDT2D : Arrays not to be processed
  --stack-method                 (FLAG) :                      : Use stack method instead of macros
  --stack84                      (FLAG) :                      : Use separate stacks for data types of sizes 4 and 8
  --style                               : NONE                 : Source code style (default: guess from file contents)
  --suffix-manyblocks                   : _MANYBLOCKS          : Suffix for many blocks routines
  --suffix-pointerparallel              : _PARALLEL            : Suffix for parallel routines
  --suffix-singlecolumn                 : _OPENACC             : Suffix for generated routines
  --tmp                                 : .                    : Temporary directory for processing
  --type-bound-methods           (FLAG) :                      : Generate & use type bound methods
  --types-constant-dir                  : types-constant       : Directory with constant type information
  --types-fieldapi-dir                  : types-fieldapi       : Directory with Field API type information
  --types-fieldapi-non-blocked   (LIST) : CPG_SL1F_TYPE,CPG_SL_MASK_TYPE : Non-blocked data types (without NPROMA)
  --use-acpy                     (FLAG) :                      : Avoid pointer aliasing using ACPY
  --use-bcpy                     (FLAG) :                      : Avoid pointer aliasing using BCPY
  --use-stack-manyblocks         (FLAG) :                      : Use stack allocation for manyblocks routines
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               
  --ydcpg_opts                   (FLAG) :                      : Change KIDIA, KFDIA -> YDCPG_OPTS, YDCPG_BNDS

```


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

```
* singleblock
  --base                         (FLAG) :                      : Base directory for file search
  --checker                      (FLAG) :                      : Sanity checks, produce a report
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --drhooktonvtx                 (FLAG) :                      : Change DrHook calls into NVTX calls
  --inlined                      (LIST) : NONE                 : List of routines to inline
  --max-statements-per-parallel         : NONE                 : Maximum number of statements per parallel section
  --merge-interfaces             (FLAG) :                      : Consider that single column interfaces and regular interfaces are in the same include file
  --only-if-newer                (FLAG) :                      : Do not update file if unchanged content
  --openmptoparallel             (FLAG) :                      : Transform OpenMP parallel sections into ACDC parallel sections
  --parallel-iterator-list       (LIST) : NONE                 : List of iterators for generating parallel sections (add to JLON, JLEV)
  --pragma                              : OpenACC              : Pragma (OpenACC or OpenMP)
  --stack-method                 (FLAG) :                      : Use stack method instead of macros
  --stack84                      (FLAG) :                      : Use separate stacks for data types of sizes 4 and 8
  --style                               : NONE                 : Source code style (default: guess from file contents)
  --suffix-singleblock                  : _SINGLEBLOCK         : Suffix for single block routines
  --suffix-singlecolumn                 : _OPENACC             : Suffix for generated routines
  --tmp                                 : .                    : Temporary directory for processing
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


This transforms a vector routine (processing a single `NPROMA` block) into a routine where each
loop on the `NPROMA` dimension is transformed into an OpenACC kernel. The result of the 
transformation is a routine which runs on the CPU, but spawns several kernels on the device.

Arguments (`NPROMA` arrays and structure holding constant data such as `YDMODEL`) 
are supposed to be present on the device when the generated routine is called. 

See [Fxtran::SingleBlock](Fxtran%3A%3ASingleBlock.md) for more details.

## manyblocks

```
* manyblocks
  --array-slice-to-address       (FLAG) :                      : Pass addresses of first array element instead of array slices
  --base                         (FLAG) :                      : Base directory for file search
  --checker                      (FLAG) :                      : Sanity checks, produce a report
  --create-interface             (FLAG) :                      : Generate an interface file
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --drhooktonvtx                 (FLAG) :                      : Change DrHook calls into NVTX calls
  --fuse-outer-dimension-names          : NONE                 : Fuse outer dimensions
  --inline-contained             (FLAG) :                      : Inline contained routines
  --inlined                      (LIST) : NONE                 : List of routines to inline
  --max-statements-per-parallel         : NONE                 : Maximum number of statements per parallel section
  --merge-interfaces             (FLAG) :                      : Consider that single column interfaces and regular interfaces are in the same include file
  --not-present-types            (LIST) : NONE                 : List of derived types not present on the device
  --only-if-newer                (FLAG) :                      : Do not update file if unchanged content
  --parallel-iterator-list       (LIST) : NONE                 : List of iterators for generating parallel sections (add to JLON, JLEV)
  --pragma                              : OpenACC              : Pragma (OpenACC or OpenMP)
  --stack-method                 (FLAG) :                      : Use stack method instead of macros
  --stack84                      (FLAG) :                      : Use separate stacks for data types of sizes 4 and 8
  --style                               : NONE                 : Source code style (default: guess from file contents)
  --suffix-manyblocks                   : _MANYBLOCKS          : Suffix for many blocks routines
  --suffix-singlecolumn                 : _OPENACC             : Suffix for generated routines
  --tmp                                 : .                    : Temporary directory for processing
  --use-stack-manyblocks         (FLAG) :                      : Use stack allocation for manyblocks routines
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


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

```
* methods
  --checker                      (FLAG) :                      : Sanity checks, produce a report
  --dir                                 : .                    : Dump result in this directory
  --field-api                    (FLAG) :                      : Dump Field API information
  --field-api-class                     : NONE                 : Field API structure category
  --method-prefix                       : ACDC_                : Prefix for method names
  --methods-list                 (LIST) : NONE                 : List of methods (copy, crc64, host, legacy, load, save, size, wipe
  --module-map                          : NONE                 : Type/module mapping for methods
  --no-allocate                  (LIST) : NONE                 : Structures that should not be allocated/deallocated
  --numbered-submodules          (FLAG) :                      : Do not generate submodules with full names, use numbers instead
  --only-components                     : NONE                 : Process only these derived type members
  --only-types                          : NONE                 : Process only these derived types
  --out                          (FLAG) :                      : Output file name
  --pragma                              : OpenACC              : Pragma (OpenACC or OpenMP)
  --skip-components                     : NONE                 : Skip these derived type members
  --skip-types                          : NONE                 : Skip these derived types
  --sorted                       (FLAG) :                      : Sort files (with number prefix) in compilation order
  --split-util                   (FLAG) :                      : Split util module into several modules (one per method)
  --tmp                                 : .                    : Temporary directory for processing
  --type-bound-methods           (FLAG) :                      : Generate & use type bound methods
  --types-constant-dir                  : types-constant       : Directory with constant type information
  --types-fieldapi-dir                  : types-fieldapi       : Directory with Field API type information
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


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

```
* interface
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --merge-interfaces             (FLAG) :                      : Consider that single column interfaces and regular interfaces are in the same include file
  --pragma                              : OpenACC              : Pragma (OpenACC or OpenMP)
  --suffix-bitrepro                     : _BITREPRO            : Suffix for bit-repro routines
  --suffix-manyblocks                   : _MANYBLOCKS          : Suffix for many blocks routines
  --suffix-pointerparallel              : _PARALLEL            : Suffix for parallel routines
  --suffix-semiimplicit                 : _SINGLEBLOCK         : Suffix for semi-implicit  routines
  --suffix-singleblock                  : _SINGLEBLOCK         : Suffix for single block routines
  --suffix-singlecolumn                 : _OPENACC             : Suffix for generated routines
  --suffix-spectral                     : _SPECTRAL            : Suffix for semi-spectral routines
  --tmp                                 : .                    : Temporary directory for processing
  --use-stack-manyblocks         (FLAG) :                      : Use stack allocation for manyblocks routines
  --ydcpg_opts                   (FLAG) :                      : Change KIDIA, KFDIA -> YDCPG_OPTS, YDCPG_BNDS

```


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

```
* bitrepro
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --inline-contained             (FLAG) :                      : Inline contained routines
  --merge-interfaces             (FLAG) :                      : Consider that single column interfaces and regular interfaces are in the same include file
  --suffix-bitrepro                     : _BITREPRO            : Suffix for bit-repro routines
  --tmp                                 : .                    : Temporary directory for processing
  --use-bit-repro-intrinsics     (FLAG) :                      : Use bit reproducible intrinsics
  --use-bit-repro-parens         (FLAG) :                      : Make sure additions are executed in the right order
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


This routine transforms the current routine into a routine where architecture dependant
intrinsics (`SIN`, `EXP`, etc.) are replaced by portable versions of these functions.

See `Fxtran::BitRepro` for more details.

## toplevel

```
* toplevel
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --method-prefix                       : ACDC_                : Prefix for method names
  --parallelmethod-section       (FLAG) :                      : Embed parallelmethod information in binary
  --suffix-pointerparallel              : _PARALLEL            : Suffix for parallel routines
  --switch                              : NONE                 : Set this variable to true if the parallel mode is enabled
  --tmp                                 : .                    : Temporary directory for processing
  --types-constant-dir                  : types-constant       : Directory with constant type information
  --types-fieldapi-dir                  : types-fieldapi       : Directory with Field API type information
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


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

## toplevelsi

```
* toplevelsi
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --parallelmethod-section       (FLAG) :                      : Embed parallelmethod information in binary
  --style                               : NONE                 : Source code style (default: guess from file contents)
  --suffix-semiimplicit                 : _SINGLEBLOCK         : Suffix for semi-implicit  routines
  --switch                              : NONE                 : Set this variable to true if the parallel mode is enabled
  --tmp                                 : .                    : Temporary directory for processing
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


## toplevelsp

```
* toplevelsp
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --parallelmethod-section       (FLAG) :                      : Embed parallelmethod information in binary
  --style                               : NONE                 : Source code style (default: guess from file contents)
  --suffix-spectral                     : _SPECTRAL            : Suffix for semi-spectral routines
  --switch                              : NONE                 : Set this variable to true if the parallel mode is enabled
  --tmp                                 : .                    : Temporary directory for processing
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


## spectral

```
* spectral
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --inline-contained             (FLAG) :                      : Inline contained routines
  --max-statements-per-parallel         : NONE                 : Maximum number of statements per parallel section
  --parallel-iterator-list       (LIST) : NONE                 : List of iterators for generating parallel sections (add to JLON, JLEV)
  --pragma                              : OpenACC              : Pragma (OpenACC or OpenMP)
  --style                               : NONE                 : Source code style (default: guess from file contents)
  --suffix-spectral                     : _SPECTRAL            : Suffix for semi-spectral routines
  --tmp                                 : .                    : Temporary directory for processing
  --user-code                           : NONE                 : User provided routine (FORTRAN file name)
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


This is the transformation for spectral (mainly horizontal diffusion) calculations.

See `Fxtran::SingleBlock::Spectral` for more details.

## idem

```
* idem
  --cycle                               : 49                   : Cycle
  --dir                                 : .                    : Dump result in this directory
  --inline-contained             (FLAG) :                      : Inline contained routines
  --style                               : NONE                 : Source code style (default: guess from file contents)
  --tmp                                 : .                    : Temporary directory for processing
  --write-metadata               (FLAG) :                      : Add metadata to generated files                               

```


Parse a file, inline some routines (optional) and write back the result.

# SEE ALSO

[fxtran-f90](fxtran-f90.md), [fxtran-gen](fxtran-gen.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
