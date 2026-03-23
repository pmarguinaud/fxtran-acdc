# NAME

[Fxtran::Style::DYNAMICS](../lib/Fxtran/Style/DYNAMICS.pm)

# DESCRIPTION

Style class for the IFS/Arpege dynamics code. This class derives from
`Fxtran::Style::IAL` and defines the naming conventions used in dynamics
routines, where the horizontal loop iterator is `JROF`, the loop bounds
are `KST` and `KEND`, and the loop size (nproma) may be given by
`KPROMA`, `YDCPG_OPTS%KLON`, or one of the `YDGEOMETRY%YRDIM%NPROMA*`
fields.

The `matchDocument` method identifies dynamics source files by the
presence of the `KST` dummy argument or local declarations of `JROF`
or `YDCPG_SL1`.

# SEE ALSO

[Fxtran::Style](Fxtran%3A%3AStyle.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
