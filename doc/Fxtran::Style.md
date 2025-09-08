# NAME

[Fxtran::Style](../lib/Fxtran/Style.pm)

# DESCRIPTION

This module implements a base class for style objects. These
are objects which contain some usefull properties to identify
the parallelism in FORTRAN source code.

Different parts of the code use slightly different notations;
for instance:

- In Meteo-France physics, the itetator and bounds of iterations are `JLON`, `KIDIA`, `KFDIA`.
- In the dynamics, the itetator and bounds of iterations are `JROF`, `KST`, `KEN`.
- In MesoNH physics, the iterator and bounds if iterations are `JI` (or `JIJ`), `D%NIB`, `D%NIE`.

All other style classes derive from `Fxtran::Style`.

This base class is also responsible for identifying the style of a FORTRAN source code 
document and create the corresponding style object.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
