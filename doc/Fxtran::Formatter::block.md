# NAME

[Fxtran::Formatter::block](../lib/Fxtran/Formatter/block.pm)

# DESCRIPTION

Base class for formatters that handle block-introducing Fortran statements
(i.e. statements that have a matching `END xxx` statement, such as
`ASSOCIATE` or `SUBROUTINE`). Inherits from [Fxtran::Formatter](Fxtran%3A%3AFormatter.md).

Provides `canonic` and `reparse` methods that temporarily append the
appropriate `END` statement so that fxtran can parse the opening statement
in context.

# SEE ALSO

[Fxtran::Formatter](Fxtran%3A%3AFormatter.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
