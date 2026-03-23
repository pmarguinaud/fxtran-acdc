# NAME

[Fxtran::Formatter::Subroutine](../lib/Fxtran/Formatter/Subroutine.pm)

# DESCRIPTION

Formatter for Fortran `SUBROUTINE` statements. Inherits from
[Fxtran::Formatter::block](Fxtran%3A%3AFormatter%3A%3Ablock.md). The `expand` method rewrites the dummy argument
list so that each argument appears on its own continuation line. The `repack`
method reassembles the argument list into a compact form respecting the
line-length limit.

# FUNCTIONS
