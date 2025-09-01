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
