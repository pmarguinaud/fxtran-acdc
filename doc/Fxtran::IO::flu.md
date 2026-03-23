# NAME

[Fxtran::IO::flu](../lib/Fxtran/IO/flu.pm)

# DESCRIPTION

FieldAPI class policy module for flux (FLU) derived types.  Provides
`skip` and `getFieldAPIMember` methods used by `Fxtran::IO` when
generating I/O code.  Components whose names end in `_B` or that
match `TYPE_XFU`, `YXFUPT`, `TYPE_CFU`, or `YCFUPT` are always
skipped.

# FUNCTIONS
