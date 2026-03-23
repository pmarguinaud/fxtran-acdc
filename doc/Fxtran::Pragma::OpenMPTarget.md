# NAME

[Fxtran::Pragma::OpenMPTarget](../lib/Fxtran/Pragma/OpenMPTarget.pm)

# DESCRIPTION

Pragma class for generating OpenMP Target directives in FORTRAN source
code. This class derives from `Fxtran::Pragma` and provides the same
interface as `Fxtran::Pragma::OpenACC` but emits `!$OMP` directives
instead of `!$ACC` directives. It is intended to be used as a drop-in
replacement when targeting OpenMP offloading instead of OpenACC.

The following directive-insertion methods are available:

- `insertParallelLoopGang`

    Inserts a `!$OMP TARGET TEAMS DISTRIBUTE` directive before a given node.

- `insertParallelLoopGangVector`

    Inserts a `!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD` directive
    before a given node.

- `insertData`

    Inserts a `!$OMP TARGET DATA` / `!$OMP END TARGET DATA` pair around a
    given node.

- `insertLoopVector`

    Inserts a `!$OMP PARALLEL DO SIMD` directive before a given node.

- `insertRoutineSeq`

    Inserts a `!$OMP DECLARE TARGET` annotation inside a subroutine.

- `insertSerial`

    Inserts a `!$OMP TARGET` / `!$OMP END TARGET` pair around a given node.

Data-movement helper methods (`enterDataCreate`, `exitDataDelete`,
`updateDevice`, `enterDataAttach`, `exitDataDetach`) return the
corresponding `!$OMP TARGET` directive strings.

# SEE ALSO

[Fxtran::Pragma](Fxtran%3A%3APragma.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
