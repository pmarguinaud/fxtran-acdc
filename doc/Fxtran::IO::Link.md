# NAME

[Fxtran::IO::Link](../lib/Fxtran/IO/Link.pm)

# DESCRIPTION

Loads FieldAPI type description files (`.pl` files produced by
`Fxtran::FieldAPI::Register`) from a directory, resolves inheritance
and cross-type references, and returns a unified data structure that
maps type names to their FieldAPI member layouts.  Also synthesises
JPRB array definitions by duplicating the corresponding JPRD
definitions.

# FUNCTIONS
