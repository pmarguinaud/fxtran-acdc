# NAME

[Fxtran::Interface](../lib/Fxtran/Interface.pm)

# DESCRIPTION

Generates Fortran interface-block files (`.intfb.h`) and MODI module wrappers
from a Fortran source file.  The core transformation strips execution parts,
contained procedures, and all declarations that are not needed to describe the
dummy argument list, keeping only the statements that are essential for the
interface.  Labels, comments (except OpenACC/OMP declare-target), includes, and
defines are also removed.  The module also supports merging an additional
OpenACC interface variant into the generated file.

# FUNCTIONS

## intfbBody

Transforms a parsed Fortran document in-place into interface-block form:
removes execution parts, contained procedures, unused declarations, labels,
comments (except OpenACC/OMP declare-target), includes, and defines, keeping
only the statements required to describe each dummy argument list.

## intfb

Generates a `.intfb.h` interface-block file from a Fortran source file.
Optionally merges an OpenACC interface variant when `merge-interfaces` is set.
Returns the path of the written file.

## modi

Generates a `modi_*.F90` MODI module wrapper from a Fortran source file,
wrapping the interface block inside a `MODULE MODI_*` / `END MODULE`.
Returns the path of the written file.
