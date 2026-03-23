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

## checkVariables

Validate variable declarations in a program unit, reporting missing INTENT
attributes, forbidden derived-type locals, assumed-shape arrays, allocatable
and pointer variables via `Fxtran::Message`.

## checkArraySyntax

Check that array-syntax assignments are limited to simple array copies or
initialisation, and that call-statement array sections are contiguous.

## checkExpressions

Verify that every NPROMA-dimensioned array reference in the execution part is
indexed with the expected loop variable (`JLON`) or range (`KIDIA:KFDIA`).

## singlecolumn

Run all three checker passes (variables, array syntax, expressions) on every
program unit in the document for the single-column parallel back-end.

## pointerparallel

Run all three checker passes on every program unit for the pointer-parallel
back-end.

## manyblocks

Run all three checker passes on every program unit for the many-blocks
back-end.

## singleblock

Run all three checker passes on every program unit for the single-block
back-end.
