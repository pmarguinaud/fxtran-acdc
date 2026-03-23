# NAME

[Fxtran::FieldAPI::Parallel](../lib/Fxtran/FieldAPI/Parallel.pm)

# DESCRIPTION

Transforms Fortran subroutines so that local NPROMA-sized arrays are
wrapped in FieldAPI array objects (`ARRAY_xD` types) suitable for
parallel execution on CPU and GPU.  The main entry point,
`wrapArrays`, locates array declarations dimensioned on `KLON` or
`YDCPG_OPTS%KLON`, replaces them with the appropriate FieldAPI
wrapper type, initialises and finalises the wrappers around the
executable section, and rewrites all array references inside parallel
regions to go through the `%P` data pointer.

# FUNCTIONS
