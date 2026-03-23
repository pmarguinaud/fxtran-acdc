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
