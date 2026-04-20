# NAME

[Fxtran::DrHook](../lib/Fxtran/DrHook.pm)

# DESCRIPTION

This module provides utilities to remove DrHook calls.

## remove

Remove all DrHook instrumentation from the given program-unit XML node `$d`.
This covers three kinds of nodes:

- `USE YOMHOOK` statements in the use-part.
- `IF (...) CALL DR_HOOK (...)` statements in the execution-part.
- Declarations of `ZHOOK_HANDLE*` variables in the declaration-part.

# SEE ALSO

[Fxtran::NVTX](Fxtran%3A%3ANVTX.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
