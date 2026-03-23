# NAME

[Fxtran::Style::ECPHYS](../lib/Fxtran/Style/ECPHYS.pm)

# DESCRIPTION

Style class for ECMWF/IFS physics routines. This class derives from
`Fxtran::Style::IAL` and defines the naming conventions used in IFS
physics code, where the horizontal loop iterator is `JL`, the loop
bounds are `KIDIA` and `KFDIA`, and the loop size (nproma) is `KLON`
or `YDGEOMETRY%YRDIM%NPROMA` or `YDCPG_OPTS%KLON`. The vertical level
iterator is `JK`.

The `matchDocument` method identifies ECPHYS source files by the
presence of a `KLON` dummy argument and a `JL` local variable
declaration (without `JLON`).
