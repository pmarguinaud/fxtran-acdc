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
