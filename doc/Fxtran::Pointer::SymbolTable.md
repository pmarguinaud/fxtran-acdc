# NAME

[Fxtran::Pointer::SymbolTable](../lib/Fxtran/Pointer/SymbolTable.pm)

# DESCRIPTION

Builds a symbol table from a parsed FORTRAN program unit, classifying each
declared variable as a FIELD API object, a constant, a subroutine argument,
a pointer, or a plain local variable. Also provides helpers to enumerate
FIELD API variables, constant variables, and to map a type specification
and rank to a FIELD API type name (e.g. `FIELD_2RB`).

# FUNCTIONS
