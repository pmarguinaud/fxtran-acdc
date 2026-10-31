# NAME

[Fxtran::Loop](../lib/Fxtran/Loop.pm)

# DESCRIPTION

Transforms NPROMA (horizontal-dimension) DO loops and array references in
Fortran source.  Given a set of variables whose first dimension is NPROMA,
this module removes the enclosing DO loop over the column index and replaces
every corresponding array subscript with a scalar index (JLON).  SUM and COUNT
idioms that operate over the column range are also simplified to scalar
expressions.  A FieldAPI variant handles pointer-accessed field objects.

# FUNCTIONS

## removeNpromaConstructs

Removes DO-loop constructs whose loop variable matches the JLON column index.
The loop header and footer are discarded and the body nodes are spliced directly
into the parent.

## replaceCall

Replaces an intrinsic call with its argument. Dropping the call also drops
the grouping it provided: when `$group` is true and the call is an operand
of another operator, the argument is enclosed in parentheses.

## replaceANY

Replaces an `ANY(...)` call with its argument, so that
`LLA .AND. ANY(B .OR. C)` does not become `LLA .AND. B .OR. C`: the argument
needs parentheses only if it is itself a logical operation.

## scalarizeArgument

Returns the single argument of a `SUM` or `COUNT` call, after replacing the
horizontal sections listed in `@range` with the scalar index `$jlon`.

Nothing is returned (and the call is left untouched) when the call has other
arguments (`DIM`, `MASK`), when it does not reduce over a horizontal section,
or when another array section remains in the argument: the call is then still
a reduction.

## fixCOUNTIdiom

Replaces `COUNT(mask(KIDIA:KFDIA))` array-reduction idioms with the scalar
equivalent `MERGE(1, 0, mask(JLON))`, where `mask` may be any logical
expression.

## fixSUMIdiom

Replaces `SUM(expr(KIDIA:KFDIA))` array-reduction idioms with the scalar
expression `expr(JLON)`, eliminating the horizontal reduction for the
single-column execution model.

## removeNpromaLoopsInSection

Applies all NPROMA-loop removal transformations to a single execution-part
section: fixes SUM and COUNT idioms, removes DO-loop constructs, and rewrites
array subscripts to scalar JLON indexing for every variable listed in
`var2dim`.

## getVarToDim

Scans the declaration part of a program unit and returns a hash ref mapping
each NPROMA-dimensioned variable name to its total number of dimensions.
When called in list context, also returns a second hash ref mapping each such
variable to the index position of the NPROMA dimension.  Pointer associations
are followed so that pointer variables inherit their target's dimension count.

## removeNpromaLoops

Top-level entry point to strip NPROMA DO loops from a program unit.  Ensures
the JLON scalar variable is declared and initialised to KIDIA, then delegates
to `removeNpromaLoopsInSection` for the full execution part.

## removeNpromaLoopsFieldAPI

Rewrites NPROMA-indexed array subscripts that are accessed through FieldAPI
pointer members (`DEVPTR` or `PTR` component references).  Looks up each
pointer variable's underlying FieldAPI type to determine the correct dimension
count, then calls `setJlon` to insert the JLON scalar index.

## setJlon

Modifies an array-expression node so that its first (or NPROMA-position)
subscript is replaced by the scalar JLON index.  If the node has no subscript
list yet, one is created with the appropriate number of `:` placeholders.
Does nothing for PRESENT() arguments and skips pointer-assignment left-hand
sides.

# SEE ALSO

[Fxtran::Decl](Fxtran%3A%3ADecl.md), [Fxtran::Scope](Fxtran%3A%3AScope.md), [Fxtran::FieldAPI](Fxtran%3A%3AFieldAPI.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
