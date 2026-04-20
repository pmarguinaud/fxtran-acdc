# NAME

[Fxtran::Pointer::SymbolTable](../lib/Fxtran/Pointer/SymbolTable.pm)

# DESCRIPTION

Builds a symbol table from a parsed FORTRAN program unit, classifying each
declared variable as a FIELD API object, a constant, a subroutine argument,
a pointer, or a plain local variable. Also provides helpers to enumerate
FIELD API variables, constant variables, and to map a type specification
and rank to a FIELD API type name (e.g. `FIELD_2RB`).

# FUNCTIONS

## getFieldAPIList

Return a list of variable names (`EN-N` nodes) whose declared type has a
corresponding `.pl` description file in `$dir` or matches `FIELD_xRB_ARRAY`.

## getConstantList

Return a list of variable names (`EN-N` nodes) whose declared type has a
corresponding `.pl` description file in the constants directory `$dir`.

## getSymbolTable

Build and return a hash-ref mapping each declared variable name to a record
describing its kind (FIELD API object, constant, pointer, subroutine argument,
or plain local), type specifier, array spec, and rank.

## getFieldType

Map a rank `$nd` and a type-specifier node `$ts` to the corresponding FIELD
API type name (e.g. `FIELD_2RB`), or return `undef` for unrecognised types.

# SEE ALSO

[Fxtran::Pointer](Fxtran%3A%3APointer.md), [Fxtran::FieldAPI](Fxtran%3A%3AFieldAPI.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
