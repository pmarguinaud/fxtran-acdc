# NAME

[Fxtran::Pointer::Parallel](../lib/Fxtran/Pointer/Parallel.pm)

# DESCRIPTION

Transforms FORTRAN program units that contain parallel-region pragmas into
code that dispatches at runtime between multiple parallelisation backends
(e.g. OpenMP, OpenACC). For each parallel region, an `IF/ELSEIF/ELSE`
construct is generated that selects the appropriate back-end based on a
runtime condition. Array references to FIELD API objects are rewritten to
use the correct view (host or device) for each target, and pointer
associations are updated accordingly.

# FUNCTIONS
