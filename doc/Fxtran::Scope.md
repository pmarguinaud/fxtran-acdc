# NAME

[Fxtran::Scope](../lib/Fxtran/Scope.pm)

# DESCRIPTION

Navigation and whitespace-management utilities for fxtran XML parse trees.
`getExec` returns the first executable statement or construct in a subtree.
`getNoExec` returns the last non-executable node that precedes the first
executable statement, which is useful as an insertion point for new
declarations or directives.  `removeWhiteSpaces` and `removeWhiteLines`
collapse redundant whitespace text nodes in the tree.

# FUNCTIONS

## getExec

Returns the first executable statement or construct node found in the subtree
rooted at `$d`, walking up through ancestor construct and statement nodes so
that the returned node is a top-level executable unit.

## getNoExec

Returns the last non-executable node that immediately precedes the first
executable statement or ACC directive in the subtree.  Useful as an insertion
point when adding declarations or directives without disturbing the executable
section.

## removeWhiteSpaces

Collapses runs of whitespace-only text nodes (spaces and non-newline
characters) in the subtree to a single newline, normalising the tree after
structural modifications.

## removeWhiteLines

Removes consecutive blank lines from whitespace-only text nodes, reducing
each run of multiple newlines to a single newline while preserving indentation
on the last line.

# SEE ALSO

[Fxtran::Decl](Fxtran%3A%3ADecl.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
