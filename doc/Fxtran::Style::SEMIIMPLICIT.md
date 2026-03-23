# NAME

[Fxtran::Style::SEMIIMPLICIT](../lib/Fxtran/Style/SEMIIMPLICIT.pm)

# DESCRIPTION

Style class for the IFS/Arpege semi-implicit solver routines. This class
derives from `Fxtran::Style::IAL` and defines the naming conventions used
in semi-implicit spectral-space code, where the horizontal loop iterator is
`JSP`, the loop bounds are `KSTA` and `KEND`, and the loop size (nproma)
is one of several `YDGEOMETRY%YRMP%NSPEC2V_*_SI` fields or `KSPEC2V`.
The vertical level iterator is `JLEV`.

The `matchDocument` method identifies semi-implicit source files by the
presence of array declarations dimensioned by `KSPEC2V` or
`YDGEOMETRY%YRMP%NSPEC2V_SI`.
