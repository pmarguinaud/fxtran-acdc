# NAME

[Fxtran::ReDim](../lib/Fxtran/ReDim.pm)

# DESCRIPTION

Removes the NPROMA (horizontal) dimension from array declarations and
references when converting to single-column layout.  For every variable whose
first dimension matches NPROMA, the module strips that dimension from the
declaration and from every array subscript in the code.  A separate helper
replaces bare `:` section subscripts with a scalar JLON index in actual
subroutine arguments that are passed as JBLK-blocked arrays.

# FUNCTIONS
