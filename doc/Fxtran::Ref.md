# NAME

[Fxtran::Ref](../lib/Fxtran/Ref.pm)

# DESCRIPTION

Low-level utilities for working with array and function reference nodes in a
fxtran XML parse tree.  Provides `parensToArrayRef` to convert a
`parens-R` function-call reference node into an `array-R` subscript node,
`resolveParensRef` to apply that conversion throughout a subtree, and
`getRLT` to retrieve (or create) the `R-LT` reference-list child of an
expression node.

# FUNCTIONS
