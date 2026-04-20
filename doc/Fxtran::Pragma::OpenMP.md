# NAME

[Fxtran::Pragma::OpenMP](../lib/Fxtran/Pragma/OpenMP.pm)

# DESCRIPTION

Pragma class for generating OpenMP directives in FORTRAN source code.
This class derives from `Fxtran::Pragma` and currently provides one
directive-insertion method:

- `parallelDo`

    Inserts a `!$OMP PARALLEL DO` directive with optional clauses (e.g.
    `PRIVATE`, `REDUCTION`) before a given node in the fxtran XML document
    tree. Long clause lists are automatically wrapped across continuation
    lines.

# SEE ALSO

[Fxtran::Pragma](Fxtran%3A%3APragma.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
