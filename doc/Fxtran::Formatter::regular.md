# NAME

[Fxtran::Formatter::regular](../lib/Fxtran/Formatter/regular.pm)

# DESCRIPTION

Base class for formatters that handle ordinary (non-block) Fortran statements.
Inherits from [Fxtran::Formatter](Fxtran%3A%3AFormatter.md). Provides `canonic` and `reparse` methods
that parse a single statement in isolation using fxtran, without needing a
matching `END` construct.
