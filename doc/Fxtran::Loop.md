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

## fixCOUNTIdiom

Replaces `COUNT(var(KIDIA:KFDIA))` array-reduction idioms with the scalar
equivalent `MERGE(1, 0, var(JLON))`, operating on the right-hand side of
assignment statements.

## fixSUMIdiom

Replaces `SUM(var(KIDIA:KFDIA))` array-reduction idioms with the scalar
element `var(JLON)`, eliminating the horizontal reduction for the
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
