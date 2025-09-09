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

[fxtran-f90](fxtran-f90.md), [fxtran-cxx](fxtran-cxx.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
