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

## reDim

Removes the NPROMA (first) dimension from array declarations and all
corresponding array subscripts in the parse tree.  When the
`redim-arguments` option is set, also replaces bare `:` first subscripts
in actual subroutine arguments with the scalar JLON index.  Variables whose
remaining dimensions are not simple literals are left unchanged.

## redimArguments

Replaces the bare `:` first subscript with the scalar JLON index in actual
subroutine arguments that are arrays whose last subscript is JBLK, targeting
the JBLK-blocked calling convention used in the outer parallel loop.
