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
