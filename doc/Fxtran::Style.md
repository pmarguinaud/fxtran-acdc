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

## newFromStyle

Create a new style object from a style name.

## newFromDocument

Create a new style object by detecting the style from a FORTRAN source document.

## new

Constructor. Dispatches to `newFromStyle` or `newFromDocument` depending on the arguments provided.

## removeUnusedIncludes

Returns 0 by default. Subclasses may override this to enable removal of unused include statements.

## noComputeRoutine

Returns 0 by default. Subclasses may override this to suppress generation of compute routines.

## preProcessForOpenACC

Pre-process a document before OpenACC conversion. Base implementation is a no-op; subclasses may override.

## customIterator

Return a custom iterator expression for this style, if any. Base implementation returns nothing.

## updateCustomIterator

Update the custom iterator in a document. Base implementation is a no-op; subclasses may override.

## getActualNproma

Return the actual nproma argument used in a program unit, by matching dummy argument names against the style's nproma list.

# SEE ALSO

[Fxtran::Style::MFPHYS](Fxtran%3A%3AStyle%3A%3AMFPHYS.md), [Fxtran::Style::IAL](Fxtran%3A%3AStyle%3A%3AIAL.md), [Fxtran::Style::ECPHYS](Fxtran%3A%3AStyle%3A%3AECPHYS.md), [Fxtran::Style::MESONH](Fxtran%3A%3AStyle%3A%3AMESONH.md).

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
