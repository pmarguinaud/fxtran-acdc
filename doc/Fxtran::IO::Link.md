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

## link

Load all `.pl` type-description files from the FieldAPI directory, resolve
supertype inheritance and cross-type component references, synthesise JPRB
variants from JPRD array definitions, and return a unified hash with keys
`types`, `update-view`, and `decls`.

## list

Return an array-ref of type names (one per `.pl` file) found in the given
directory.

# SEE ALSO

[Fxtran::IO](Fxtran%3A%3AIO.md), [Fxtran::FieldAPI](Fxtran%3A%3AFieldAPI.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
