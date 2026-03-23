# NAME

[Fxtran::IO::sta](../lib/Fxtran/IO/sta.pm)

# DESCRIPTION

FieldAPI class policy module for state (STA) derived types.  Provides
`skip` and `getFieldAPIMember` methods used by `Fxtran::IO` when
generating I/O code.  Only pointer components are examined; the
special component `P` is always skipped.

# FUNCTIONS
