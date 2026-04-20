# NAME

[Fxtran::Module](../lib/Fxtran/Module.pm)

# DESCRIPTION

Utilities for manipulating Fortran MODULE program units in a parsed document.
Provides functions to append a suffix to a module name (updating both the
MODULE and END MODULE statements) and to rename a module using a user-supplied
transformation function.

# FUNCTIONS

## addSuffix

Appends `$suffix` to the module name in both the MODULE and END MODULE
statements of the parse tree node `$d`.

## rename

Renames a module by applying the user-supplied transformation function `$sub`
to the current module name, then updating the MODULE and END MODULE statement
nodes in the parse tree.

# SEE ALSO

[Fxtran::Subroutine](Fxtran%3A%3ASubroutine.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
