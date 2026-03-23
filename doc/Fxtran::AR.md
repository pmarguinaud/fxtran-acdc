# NAME

[Fxtran::AR](../lib/Fxtran/AR.pm)

# DESCRIPTION

This module provides the `expandObjects` function, which processes
a list of arguments.

Arguments with the `.o` extension may actually be archives (if the `.o`
file was created by `fxtran-f90` with the `--object-merge-method` option
set to `archive`).

In this case, the archive is expanded into single objects and these objects
replace the item in the argument list.

# SEE ALSO

[fxtran-f90](fxtran-f90.md), [fxtran-cxx](fxtran-cxx.md), [Fxtran::F90Compiler](Fxtran%3A%3AF90Compiler.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

## sysAR

Run the `ar` archiver with the given arguments. If the first argument is `t`,
returns the list of members in the archive. Otherwise, runs the command and
dies on failure.

## expandObjects

Expand `.o` arguments that are actually archives (`application/x-ar`) into
their individual object files. Non-archive `.o` files and non-object arguments
are passed through unchanged. The `$args` array ref is modified in place.

## new

Constructor for `Fxtran::AR::Object::Temp`. Accepts `dir` and `file`
keyword arguments and returns a blessed hash reference.

## asString

Returns the full path of the temporary object file by joining `dir` and
`file`. Used as the string overload for `Fxtran::AR::Object::Temp`.
