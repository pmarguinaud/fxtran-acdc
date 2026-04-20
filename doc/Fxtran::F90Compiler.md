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

## touch

Update the access and modification timestamps of one or more files. If the time argument is negative, the current time is used.

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

## slurp

Read all lines of a file and return them as a list.

## study

Analyse a FORTRAN source file to extract the modules it defines and the modules it uses. Returns a hash with keys `mod` and `use`.

## obj

Derive the object file name from a source file name.

## make

Generate a Makefile for the given sources and invoke `make` to build a library or object file.

## sortFilesByLevel

Sort FORTRAN source files by dependency level so that files with no dependencies come first.

## concatenateSource

Concatenate all FORTRAN source files into a single file (in dependency order) and compile it.

## concatenateIncludeSource

Concatenate FORTRAN source files via `#include` directives into a single file (in dependency order) and compile it.

# SEE ALSO

[Fxtran::AR](Fxtran%3A%3AAR.md), [Fxtran::Tool](Fxtran%3A%3ATool.md).

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

# POD ERRORS

Hey! **The above document had some coding errors, which are explained below:**

- Around line 547:

    &#x3d;cut found outside a pod block.  Skipping to next block.
