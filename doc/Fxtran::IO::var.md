# NAME

[Fxtran::IO::var](../lib/Fxtran/IO/var.pm)

# DESCRIPTION

FieldAPI class policy module for variable (VAR) derived types.
Provides `skip` and `getFieldAPIMember` methods used by
`Fxtran::IO` when generating I/O code.  Only pointer components are
examined; the special component `P` is mapped to `T0` before
looking for the corresponding FieldAPI member (using the `F` prefix
rather than `F_`).
