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
