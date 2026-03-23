# NAME

[Fxtran::Compare](../lib/Fxtran/Compare.pm)

# DESCRIPTION

Utilities for comparing Fortran source files and directories.  Files are
parsed with fxtran into canonical form before comparison so that insignificant
formatting differences are ignored.  An interactive prompt mode allows the user
to overwrite the reference file when a difference is detected.

# FUNCTIONS

## compareFiles

Compares two Fortran source files by parsing each into canonical form and
running `diff`.  With the `compare-prompt` option the user is offered the
choice to overwrite the first file with the second when a difference is found.

## compareDirectories

Compares all `*.F90` files found in two directories by calling `compareFiles`
on each matched pair.

## compare

Dispatches to `compareDirectories` or `compareFiles` depending on whether the
two arguments are directories or files.
