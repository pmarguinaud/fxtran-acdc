# NAME

[Fxtran::Pragma::OpenACCToOpenMPTarget](../lib/Fxtran/Pragma/OpenACCToOpenMPTarget.pm)

# DESCRIPTION

Utility module for converting OpenACC directives to OpenMP Target
directives in an fxtran XML document tree. It also provides optional
expansion of combined `!$ACC PARALLEL LOOP` directives and optional
wrapping of directive lines in `#ifdef` guards.

The module contains three packages:

- `Fxtran::Pragma::OpenACCToOpenMPTarget`

    The top-level driver. The `apply` method orchestrates the full
    transformation pipeline: cleaning up `!$ACC PARALLEL LOOP` constructs,
    optionally expanding them into separate `!$ACC PARALLEL` and `!$ACC
    LOOP` directives, optionally converting everything to `!$OMP TARGET`
    syntax, and optionally wrapping directive lines in
    `#ifdef _FXTRAN_ACDC_USE_OPENACC` or
    `#ifdef _FXTRAN_ACDC_USE_OPENMPTARGET` guards.

- `Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::clause`

    Handles clause-level conversion from OpenACC to OpenMP Target. Each
    method corresponds to an OpenACC clause name and rewrites it to the
    equivalent OpenMP Target clause (e.g. `gang` becomes
    `TEAMS DISTRIBUTE`, `vector` becomes `PARALLEL DO SIMD`,
    `create` becomes `MAP(ALLOC:...`).

- `Fxtran::Pragma::OpenACCToOpenMPTarget::openacc::directive`

    Handles directive-level conversion from OpenACC to OpenMP Target. Each
    method corresponds to an OpenACC directive name and rewrites it to the
    equivalent OpenMP Target directive (e.g. `parallel` becomes
    `TARGET TEAMS`, `serial` becomes `TARGET`, `data` becomes
    `TARGET DATA`).

# SEE ALSO

[Fxtran::Pragma](Fxtran%3A%3APragma.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
