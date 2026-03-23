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

## parensToArrayRef

Converts a `parens-R` function-call reference node into an `array-R`
subscript node in-place by renaming the node and its child list, and wrapping
each element in a `lower-bound` child so it becomes a proper section subscript.

## resolveParensRef

Walks all direct `parens-R` children of the given node and converts each
ambiguous one (i.e. not a genuine function call whose name starts with `F`)
to an `array-R` subscript node via `parensToArrayRef`.

## getRLT

Returns the `R-LT` (reference list) child of an expression node, creating
and appending an empty one if it does not already exist.
