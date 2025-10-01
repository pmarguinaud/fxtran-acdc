# NAME

[Fxtran::Formatter](../lib/Fxtran/Formatter.pm)

# DESCRIPTION

This module provides a framework for reformatting particular statements.

Adding a capability for a new statement type involves adding a new class, inheriting
from `Fxtran::Formatter::regular` or `Fxtran::Formatter::block`.

The new class must implement the following methods:

- expand

    Transform the statement into a single line, plain statement, making simplifications
    when possible.

- repack

    Transform a packed statement into a multiline statement.

The module also provides unit wide reformatting:

- simplifyAssociateBlocks

    Remove unused `ASSOCIATE` selectors.

- alignUseStatements

    Align use statements, remove unused imported entities.

- alignArgumentDeclarations

    Align argument declarations statements.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

# SEE ALSO

[fxtran-formatter](fxtran-formatter.md), [Fxtran::Formatter::regular](Fxtran%3A%3AFormatter%3A%3Aregular.md), [Fxtran::Formatter::block](Fxtran%3A%3AFormatter%3A%3Ablock.md)
