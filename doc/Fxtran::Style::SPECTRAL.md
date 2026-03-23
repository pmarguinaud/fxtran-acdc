# NAME

[Fxtran::Style::SPECTRAL](../lib/Fxtran/Style/SPECTRAL.pm)

# DESCRIPTION

Style class for IFS/Arpege spectral-space routines (horizontal diffusion
and related transforms). This class derives from `Fxtran::Style::IAL` and
defines the naming conventions for spectral code, where the horizontal loop
iterator is `JSP`, the loop bounds are `KSTA` and `KEND`, and the loop
size (nproma) is one of several `YDGEOMETRY%YRMP%NSPEC2_*_HD` fields.
The vertical level iterator is `JLEV`.

The `matchDocument` method always returns false; spectral-style files are
not auto-detected and must be selected explicitly.

# SEE ALSO

[Fxtran::Style](Fxtran%3A%3AStyle.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
