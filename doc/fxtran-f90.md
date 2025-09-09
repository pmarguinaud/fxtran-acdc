`fxtran-f90` is compiler wrapper performing the following tasks:

- Apply some transformations on user-written source code and generate FORTRAN source code ready 
for GPU accelerators.
- Compile the original code and code produced at previous step.
- Link all objects into a single one.

`fxtran-f90` relies on `fxtran` for parsing and transforming FORTRAN source code.
# NAME

[fxtran-f90](../bin/fxtran-f90)

# SYNOPSIS

    $ fxtran-f90 --config fxtran.conf [.. fxtran generation options ..] -- pgf90 [.. PGI options ..] file.F90

# DESCRIPTION

`fxtran-f90` is compiler wrapper performing the following tasks:

- Apply some transformations on user-written source code and generate FORTRAN source code ready 
for GPU accelerators.
- Compile the original code and code produced at previous step.
- Link all objects into a single one.

`fxtran-f90` relies on `fxtran` for parsing and transforming FORTRAN source code.

# OPTIONS

`fxtran-f90` accepts all options of `fxtran-gen`, and the following options:

- --dryrun 

    Transform the code, dump the result into files, but does not compile the produced
    source code.

- --debug

    Do not remove intermediate files and temporary directories.

- --package

    Package used for generation (defaults to `Fxtran::Generate`).

- --method

    Force a transformation method; the default is to read the transformation method
    from the FORTRAN source code (this should be an ACDC directive).

- --config

    Read options from a config file. Please note that options passed before --config are 
    superseded by options from the configuration file. Likewise, options from the configuration
    file are overridden by options passed after the configuration file.

- --user-directory-in

    This option takes a directory name as input. If a transformation produces a file and 
    this file exists in this directory (provided by `--user-directory-in`), then the file
    from this directory is compiled. This is used for debugging.

- --user-directory-out

    This option takes a directory name where results from transformations are written. This
    allows for inspection of generated source code by the user.

- --prefix

    This option takes a directory name where fxtran-acdc libraries and headers are installed. These
    libraries are compiled using `fxtran-boot`.

- --object-merge-method

    Method used to produce a single object file. Can be either 'link' (compile in parallel 
    and use the linker to merge the two objects), 'concatenate' (concatenate source
    code before compiling), or 'archive' (merge objects into an archive).

Options passed to `fxtran-f90` must be followed by a double hyphen (`--`); then come the
compiler name followed by the compiler options and files to be processed.

Note that `fxtran-f90` will pass fxtran-acdc libraries and includes to the compiler, and will define 
the following macros:

- `FXTRAN_ACDC`
- `FXTRAN_FILE`
- `FXTRAN_LINE`

# EXAMPLES

## DIRECTIVE INSTRUMENTED FILE

    SUBROUTINE ACTKE ( YDCST, YDLDDH, YDMDDH, YDML_PHY_MF, KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV,      &
    & PAPHI, PAPHIF, PAPRS, PAPRSF, PDELP, PR, PT, PU, PV, PQ, PQICONV,  PQLCONV, PLSCPE, PCD, PCH, PGZ0, &
    & PTS, PQS, PQICE, PQLI, PECT, PPRODTH,  PNLAB, PNLABCVP, PKTROV, PKQROV, PKQLROV, PKUROV, PXTROV,    &
    & PXUROV, PNBVNO,  PNEBS, PQCS, PNEBS0, PQCS0, PCOEFN, PFECT, PFECTI, PECT1, PTPRDY, PEDR,  YDDDH)
    
    !$ACDC singlecolumn 
    
    USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
    ...
    USE DDH_MIX              , ONLY : ADD_FIELD_3D, TYP_DDH, NEW_ADD_FIELD_3D
    
    IMPLICIT NONE
    
    TYPE(TCST)                  ,INTENT(IN)     :: YDCST
    TYPE(TLDDH)                 ,INTENT(IN)     :: YDLDDH
    ...
    INTEGER(KIND=JPIM)          ,INTENT(IN)     :: KLEV 
    REAL(KIND=JPRB)             ,INTENT(IN)     :: PAPHI(KLON,0:KLEV)
    REAL(KIND=JPRB)             ,INTENT(IN)     :: PAPHIF(KLON,KLEV)
    REAL(KIND=JPRB)             ,INTENT(IN)     :: PAPRS(KLON,0:KLEV)

This routine will be processed using the singlecolumn method. `fxtran-f90` must be invoked as follows:

    $ ls -l actke.F90
    -rw-r--r-- 1 marguina algo 15619  Aug  23 20:14 actke.F90
    $ fxtran-f90 --dryrun -- f90 -c actke.F90
    $ ls -l actke_openacc.F90
    -rw-r--r-- 1 marguina algo 9580  Aug  23 20:17 actke_openacc.F90

The file `actke_openacc.F90` has been created by the singlecolumn method of the `Fxtran::Generate` module.

## INTERFACE GENERATION

We use the same file as in the previous example:

    $ fxtran-f90 --method interface --dryrun -- f90 -c actke.F90
    $ ls -l actke.intfb.h
    -rw-r--r-- 1 marguina algo 3984  Aug  23 20:19 actke.intfb.h

`actke.intfb.h` contains the interface of the routine `actke.F90`.

We can ask to generate the interface of `actke_openacc.F90`:

    $ fxtran-f90 --merge-interfaces --method interface --dryrun -- f90 -c actke.F90
    $ ls -l actke.intfb.h
    -rw-r--r-- 1 marguina algo 7278 Aug 23 20:21 actke.intfb.h

`actke.intfb.h` now contains both the interface of `actke.F90` and of `actke_openacc.F90`.

# SEE ALSO

[fxtran](https://github.com/pmarguinaud/fxtran), [fxtran-gen](fxtran-gen.md), [fxtran-boot](fxtran-boot.md), FORTRAN, XML, [Fxtran::F90Compiler](Fxtran%3A%3AF90Compiler.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
