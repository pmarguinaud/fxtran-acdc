# NAME

[Fxtran::Formatter::Call](../lib/Fxtran/Formatter/Call.pm)

# DESCRIPTION

Formatter for Fortran `CALL` statements. Inherits from
[Fxtran::Formatter::regular](Fxtran%3A%3AFormatter%3A%3Aregular.md). The `expand` method rewrites the statement so
that each actual argument appears on its own continuation line. The `repack`
method reassembles the argument list into a compact form that respects the
line-length limit.

# FUNCTIONS

## expand

Takes a Fortran `CALL` statement node and an indentation string. Extracts the
procedure name and the actual argument list, then rewrites the statement so
that each argument appears on its own continuation line. Returns the re-parsed
statement node.

## repack

Takes an expanded `CALL` statement node and an indentation string. Extracts
the procedure designator and the argument list, then reassembles them into a
compact form that fits within the line-length limit via
`repackCallLikeStatement`. Returns the reformatted statement.

# SEE ALSO

[Fxtran::Formatter](Fxtran%3A%3AFormatter.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
