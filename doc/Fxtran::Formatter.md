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

# SEE ALSO

[Fxtran::Formatter::Call](Fxtran%3A%3AFormatter%3A%3ACall.md), [Fxtran::Formatter::Associate](Fxtran%3A%3AFormatter%3A%3AAssociate.md), [Fxtran::Formatter::Subroutine](Fxtran%3A%3AFormatter%3A%3ASubroutine.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

# SEE ALSO

[fxtran-formatter](fxtran-formatter.md), [Fxtran::Formatter::regular](Fxtran%3A%3AFormatter%3A%3Aregular.md), [Fxtran::Formatter::block](Fxtran%3A%3AFormatter%3A%3Ablock.md)

## getIndent

Return the indentation level (in characters) of a given XML node by inspecting preceding text nodes.

## runcommand

Execute a system command given as an array reference in `$args{cmd}`; die on failure.

## getDocument

Parse a Fortran source file with fxtran and return the resulting XML document.

## simplifyAssociateBlocks

Remove unused selectors from `ASSOCIATE` blocks in a document node, unwrapping the block entirely when all selectors are unused.

## loadClass

Attempt to load a Perl class by name and cache the result; return the class name on success or `undef` if the module is not found.

## class

Derive the formatter class name for a statement node and return it if the class can be loaded, or `undef` otherwise.

## prepareFileForMerging

Expand all formattable statements in a Fortran file to single-line form in preparation for a merge operation, optionally simplifying ASSOCIATE blocks first.

## repackStatementsAfterMerge

Re-format (repack) all statements in a Fortran file after a merge, writing the result back in place.

## formatStatements

Apply all requested formatting transformations to a Fortran file (simplify ASSOCIATE, align USE/declarations, remove unused locals, repack statements) and write the output.

## alignUseStatements

Remove unused imported entities from USE statements in a program unit and reformat them with aligned module names.

## alignArgumentDeclarations

Reformat argument declaration statements in a program unit so that type specifiers and attributes are column-aligned.

## repackStatements

Repack (reformat to multiline) all formattable statements in a document by running canonic normalisation followed by the per-class `repack` method.

## repackCallLikeStatement

Build a multiline continuation string for a call-like statement (CALL or similar), splitting argument lists at a configurable line-length limit, and return the reparsed node.

## removeEnDecl

Remove a single entity declaration node and its surrounding comma/separator nodes from a declaration statement, removing the whole statement if it becomes empty.

## removeArg

Remove a dummy argument node and its adjacent separator nodes from a subroutine argument list.

## removeUnusedLocalVariables

Remove local variable declarations (and optionally unused dummy arguments) that are never referenced in the execution part of a program unit.
