# NAME

[Fxtran::Pragma::OpenACC](../lib/Fxtran/Pragma/OpenACC.pm)

# DESCRIPTION

Pragma class for generating and manipulating OpenACC directives in
FORTRAN source code. This class derives from `Fxtran::Pragma` and
provides methods to insert, expand, and clean up `!$ACC` directives in
an fxtran XML document tree.

The following directive-insertion methods are available:

- `insertParallelLoopGang`

    Inserts a `!$ACC PARALLEL LOOP GANG` directive before a given node.

- `insertParallelLoopGangVector`

    Inserts a `!$ACC PARALLEL LOOP GANG VECTOR` directive before a given node.

- `insertData`

    Inserts a `!$ACC DATA` / `!$ACC END DATA` pair around a given node.

- `insertLoopVector`

    Inserts a `!$ACC LOOP VECTOR` directive before a given node.

- `insertRoutineVector`

    Inserts a `!$ACC ROUTINE (...) VECTOR` annotation inside a subroutine.

- `insertRoutineSeq`

    Inserts a `!$ACC ROUTINE (...) SEQ` annotation inside a subroutine.

- `insertSerial`

    Inserts a `!$ACC SERIAL` / `!$ACC END SERIAL` pair around a given node.

Data-movement helper methods (`enterDataCreate`, `exitDataDelete`,
`updateDevice`, `enterDataAttach`, `exitDataDetach`) return the
corresponding `!$ACC` directive strings.

The `expandParallelLoop` and `expandParallelData` utility functions
split a combined `!$ACC PARALLEL LOOP` directive into separate
`!$ACC PARALLEL` and `!$ACC LOOP` directives, and extract data clauses
into a dedicated `!$ACC DATA` region respectively.
