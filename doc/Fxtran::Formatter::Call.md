# NAME

[Fxtran::Formatter::Call](../lib/Fxtran/Formatter/Call.pm)

# DESCRIPTION

Formatter for Fortran `CALL` statements. Inherits from
[Fxtran::Formatter::regular](Fxtran%3A%3AFormatter%3A%3Aregular.md). The `expand` method rewrites the statement so
that each actual argument appears on its own continuation line. The `repack`
method reassembles the argument list into a compact form that respects the
line-length limit.

# FUNCTIONS
