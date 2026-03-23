# NAME

[Fxtran::Inline](../lib/Fxtran/Inline.pm)

# DESCRIPTION

The purpose of this module is to provide functions to inline subroutine calls.

# FUNCTIONS

## replaceDummyArgumentByActual

Dispatch replacement of a dummy argument expression `$e` by its actual argument `$a`, branching on whether `$a` is a named expression, an operator expression, or a literal.

## replaceDummyArgumentByActualOpE

Replace a dummy argument expression node with a parenthesised copy of the actual operator expression; die if the dummy carries array references.

## resolveArrayRef

Substitute colon section-subscripts in an actual array reference `$ra` with the corresponding subscripts from the dummy array reference `$re`, then replace `$re` in the tree with the updated `$ra`.

## replaceDummyArgumentByActualNamedE

Replace a dummy named-expression node with its actual named-expression equivalent, resolving component references and array section subscripts as needed.

## removeStmt

Remove a statement node and all surrounding whitespace/text nodes up to and including the end-of-line, trimming the preceding newline to keep formatting clean.

## inlineSingleCall

Inline a single CALL statement by replacing dummy arguments with actuals, removing dummy declarations, merging USE/declarations from the callee into the caller, and inserting the callee's executable statements in place of the call.

## loopElementalSingleCall

Wrap a single call to an ELEMENTAL subroutine with DO loops over the array dimensions of its array arguments, replacing whole-array section subscripts with loop indices.

## loopElemental

Add DO loops around all calls to an ELEMENTAL subroutine named `$n2` within program unit `$d1`.

## inlineContainedSubroutine

Inline all calls to the contained subroutine named `$n2` within program unit `$d1`, adding DO loops first if the subroutine is ELEMENTAL.

## sortContainedSubroutines

Sort a list of contained subroutine name nodes so that subroutines called by no other contained subroutine are inlined first, using a dependency-counting approach.

## inlineContainedSubroutines

Inline all routines from the CONTAINS section. Sort these
routines: inline first routines which are not called by
any other CONTAINed subroutine.

Remove the CONTAINS statement and the CONTAINed subroutines.

## suffixVariables

Append a suffix to the names of all local variables (excluding dummy arguments and style-defined loop indices) in a program unit, updating both declarations and all references.

## inlineExternalSubroutine

Inline external subroutine. Suffix inlined routine variables with inlined routine name.

# SEE ALSO

[Fxtran::Generate](Fxtran%3A%3AGenerate.md), [Fxtran::Include](Fxtran%3A%3AInclude.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
