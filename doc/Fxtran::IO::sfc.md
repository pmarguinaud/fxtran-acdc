# NAME

[Fxtran::IO::sfc](../lib/Fxtran/IO/sfc.pm)

# DESCRIPTION

FieldAPI class policy module for surface (SFC) derived types.
Provides `skip` and `getFieldAPIMember` methods used by
`Fxtran::IO` when generating I/O code.  Components matching
`P.*_T[019]` (tiled pointer arrays) or named `F_GROUP`,
`VARIABLE_GROUP`, or `PGROUP` are unconditionally skipped.

# FUNCTIONS
