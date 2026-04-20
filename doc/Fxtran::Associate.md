# NAME

[Fxtran::Associate](../lib/Fxtran/Associate.pm)

# DESCRIPTION

This module provides the `resolveAssociates` function, which replaces associated expressions
with their contents. All `ASSOCIATE` blocks are scanned and selectors are replaced 
by actual expressions in the block.

The main argument of `resolveAssociates` is an XML node which maps to a FORTRAN entity
such as a program unit or something smaller like a `DO` loop or something else.

The `outer` option allows for processing outer `ASSOCIATE` blocks (those outside the 
section we are working with). 

`ASSOCIATE` constructs are removed when the outer option is not active.

# LIMITATIONS

`ASSOCIATE` selectors other than named expressions are not supported; for instance:

    ASSOCIATE (X => 1 + 2)

`ASSOCIATE` selectors mapped to complex slices are not supported; for instance:

    TYPE TT
      REAL :: X
    END TYPE

    TYPE (TT) :: YY (N)

    ASSOCIATE (XX => YY%X)

## resolveAssociates

Resolve all `ASSOCIATE` constructs reachable from the given XML node `$d`.
Each associate selector is inlined - occurrences of the associate name inside
the block are replaced by the associated expression, including any additional
references (array subscripts, component accesses) appended to the name.

When the `outer` option is set, only `ASSOCIATE` blocks that are ancestors
of `$d` (i.e. outside the current scope) are processed and are not removed.
Without `outer`, the `ASSOCIATE` construct itself is removed after inlining.

# SEE ALSO

[Fxtran::Call](Fxtran%3A%3ACall.md), [Fxtran::Formatter](Fxtran%3A%3AFormatter.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
