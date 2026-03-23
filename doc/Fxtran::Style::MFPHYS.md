# NAME

[Fxtran::Style::MFPHYS](../lib/Fxtran/Style/MFPHYS.pm)

# DESCRIPTION

Style class for Meteo-France physics routines. This class derives from
`Fxtran::Style::IAL` and defines the naming conventions used in
Meteo-France physics code, where the horizontal loop iterator is `JLON`,
the loop bounds are `KIDIA` and `KFDIA`, and the loop size (nproma) is
`KLON` or `YDGEOMETRY%YRDIM%NPROMA`. The vertical level iterator is
`JLEV`.

The `matchDocument` method identifies MFPHYS source files by the presence
of a `KLON` dummy argument (or the combination of `KIDIA`, `KFDIA` and
`YDGEOMETRY`) together with a `JLON` local variable declaration.
