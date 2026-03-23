# NAME

[Fxtran::Generate::Checker](../lib/Fxtran/Generate/Checker.pm)

# DESCRIPTION

Validation pass used by the `Fxtran::Generate` framework.  After a
parallel-method transformation has been applied, this module checks that the
resulting program unit conforms to the coding rules required by the target
parallel back-end (single-column, pointer-parallel, many-blocks, or
single-block).  It reports violations via [Fxtran::Message](Fxtran%3A%3AMessage.md) for:

- missing INTENT attributes, forbidden derived-type locals, assumed-shape
arrays, allocatable and pointer variables (`checkVariables`).
- array-syntax assignments or call statements that are not simple array
copies or contiguous sections (`checkArraySyntax`).
- NPROMA-dimensioned arrays that are not indexed with the expected loop
index or range (`checkExpressions`).

# FUNCTIONS
