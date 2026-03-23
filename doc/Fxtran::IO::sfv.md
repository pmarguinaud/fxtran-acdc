# NAME

[Fxtran::IO::sfv](../lib/Fxtran/IO/sfv.pm)

# DESCRIPTION

FieldAPI class policy module for surface variable (SFV) derived types.
Provides `skip` and `getFieldAPIMember` methods used by
`Fxtran::IO` when generating I/O code.  The same tiled-pointer and
group-name components as `Fxtran::IO::sfc` are skipped.  When
looking up the corresponding FieldAPI member, the leading character of
the component name is stripped before prepending `F_`.

# FUNCTIONS
