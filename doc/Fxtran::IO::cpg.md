# NAME

[Fxtran::IO::cpg](../lib/Fxtran/IO/cpg.pm)

# DESCRIPTION

FieldAPI class policy module for CPG (column physics grid) derived
types.  Provides `skip` and `getFieldAPIMember` methods used by
`Fxtran::IO` when generating I/O code.  In addition to the standard
pointer check, this module unconditionally skips components named
`ZVIEW`, `F_DATA`, and `ZDATA`.

# FUNCTIONS
