# NAME

[Fxtran::Pointer](../lib/Fxtran/Pointer.pm)

# DESCRIPTION

This module provides utilities to manage pointer variables, and transform them to 
CRAY pointers.

# FUNCTIONS

## setPointersDimensions

Look for pointer variables associated to regular FORTRAN arrays and replace implicit 
shapes array specs with full fledged dimensions (dimensions are taken from pointees).

Return the list of pointer variables.

## handleAssociations

Replace associations with fxtran macros. Replace `NULLIFY` statements with fxtran macros.

# AUTHOR

philippe.marguinaud@meteo.fr

# SEE ALSO

# COPYRIGHT

Meteo-France 2025
