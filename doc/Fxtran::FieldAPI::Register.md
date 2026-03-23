# NAME

[Fxtran::FieldAPI::Register](../lib/Fxtran/FieldAPI/Register.pm)

# DESCRIPTION

Scans Fortran type definitions in an fxtran XML document and records
which components are backed by FieldAPI objects.  For each type
construct found, `registerFieldAPI1` collects component metadata
(pointer attributes, array rank, type specifier, and inheritance) and
`registerFieldAPI` serialises the resulting hash to a per-type `.pl`
file under the directory given by the `types-fieldapi-dir` option.
An optional class (e.g. `Fxtran::IO::cpg`) can be supplied via the
`field-api-class` option to customise how FieldAPI members are
identified for a particular family of types.

# FUNCTIONS

## registerFieldAPI1

Parse a single `T-construct` node and return a hash describing the type's
name, supertype, FieldAPI component layout (pointers, rank, type specifier),
and whether it has an `UPDATE_VIEW` procedure.

## registerFieldAPI

Scan all `T-construct` nodes in document `$d`, call `registerFieldAPI1` for
each non-skipped type, and serialise the resulting metadata hash to a `.pl`
file in the directory given by `types-fieldapi-dir`.

# SEE ALSO

[Fxtran::FieldAPI](Fxtran%3A%3AFieldAPI.md), [Fxtran::FieldAPI::Parallel](Fxtran%3A%3AFieldAPI%3A%3AParallel.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
