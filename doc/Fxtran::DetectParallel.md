# NAME

[Fxtran::DetectParallel](../lib/Fxtran/DetectParallel.pm)

# DESCRIPTION

This module provides the routine `createParallelSections` whose
purpose is to detect parallel loops (loops on NPROMA), and add
ACDC parallel sections.

Adjacent parallel sections can be merged if they do not contain
CALL statements and the number of statements involving calculations
do not exceed the `max-statements-per-parallel` argument.

The `parallel-iterator-list` contain the list of iterators
of loops that should be included in parallel sections (NPROMA and
vertical iterators are the default).

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

## createParallelSections

Scan a program unit (or execution-part node) for assignment statements that
involve arrays dimensioned along the NPROMA axis and wrap each qualifying
`DO` loop or array-syntax statement in a `parallel-section` XML element.

Adjacent parallel sections that contain no `CALL` statements and whose
combined statement count does not exceed `max-statements-per-parallel` are
merged into a single section.  The set of loop iterators that qualify for
parallelisation is controlled by the `parallel-iterator-list` option
(defaulting to the NPROMA and vertical iterators).
