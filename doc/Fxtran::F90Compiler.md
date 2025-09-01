# NAME

[Fxtran::F90Compiler](../lib/Fxtran/F90Compiler.pm)

# DESCRIPTION

The purpose of this module is to provide utilities to wrap the 
compiler and compile sets of files.

# PUBLIC FUNCTIONS

## compile

This function compiles FORTRAN and C files which are present in
the current directory. Dependencies between FORTRAN files are detected.
A `Makefile` is generated and the compilation run with make if
several files have to be compiled.

## run

This function compiles files passed as arguments, using the specified compilers. Named options are:

- F90

    list of FORTRAN files

- C

    list of C files.

- CXX

    list of C++ files.

- dryrun

    Stop the process just before compilation.

- user-directory-in

    Substitute files to be compiled with files from this directory. Used for debugging.

- user-directory-out

    Save files from current directory (mostly generated code) into this directory.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
