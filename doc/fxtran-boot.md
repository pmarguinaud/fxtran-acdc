`fxtran-boot` compiles and install the fxtran-acdc libraries. Compilers and
their options may be passed directly as options, or inferred from the `ecbuild`
environment.
# NAME

[fxtran-boot](../bin/fxtran-boot)

# SYNOPSIS

    $ fxtran-boot \
       --prefix <path-to-fxtran-acdc-installation> \
       --F90 "FORTRAN compiler & options" \
       --CC "C compiler & options" \
       --CXX "C++ compiler & options"

or (ecbuild/cmake environment):

    $ fxtran-boot \
       --prefix <path-to-fxtran-acdc-installation> \
       --ecbuild-build-type BIT

# DESCRIPTION

`fxtran-boot` compiles and install the fxtran-acdc libraries. Compilers and
their options may be passed directly as options, or inferred from the `ecbuild`
environment.

# OPTIONS

- --prefix

    Directory where fxtran-acdc libraries and headers should be installed.

- --F90

    FORTRAN compiler and options.

- --CC

    C compiler and options.

- --CXX

    C++ compiler and options.

- --ecbuild-build-type

    `ecbuild` build type (similar to cmake build type).

# SEE ALSO

[fxtran-f90](fxtran-f90.md), [ecbuild](https://github.com/ecmwf/ecbuild), [Fxtran::F90Compiler](Fxtran%3A%3AF90Compiler.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
