# NAME

[Fxtran::Formatter::Subroutine](../lib/Fxtran/Formatter/Subroutine.pm)

# DESCRIPTION

Formatter for Fortran `SUBROUTINE` statements. Inherits from
[Fxtran::Formatter::block](Fxtran%3A%3AFormatter%3A%3Ablock.md). The `expand` method rewrites the dummy argument
list so that each argument appears on its own continuation line. The `repack`
method reassembles the argument list into a compact form respecting the
line-length limit.

# FUNCTIONS

## expand

Takes a Fortran `SUBROUTINE` statement node and an indentation string.
Normalises the statement to its canonical form, then rewrites the dummy
argument list so that each argument appears on its own continuation line.
Returns the re-parsed statement node.

## repack

Takes an expanded `SUBROUTINE` statement node and an indentation string.
Extracts the subroutine name and the dummy argument list, then reassembles
them into a compact multi-line form that respects the line-length limit via
`repackCallLikeStatement`.

# SEE ALSO

[Fxtran::Formatter](Fxtran%3A%3AFormatter.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
