# NAME

[Fxtran::UserCode](../lib/Fxtran/UserCode.pm)

# DESCRIPTION

Instead of generating code for a routine, look for user provided code (option `user-code`).

The user provided code is renamed according using the appropriate routine name and suffix.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

## getUserCode

Load user-provided replacement code instead of generating it automatically.
Both the original source file (`$F90`) and the user-supplied file (resolved
via the `user-code` option) are parsed.  When a method suffix is configured,
every program-unit name in the user file is renamed to match the original
name with the suffix appended, and any `DR_HOOK` string literals that
reference the routine name are updated accordingly.  The resulting text is
written to `$F90out` and, if `FXTRAN_F90_COMMAND` is set in the
environment, a dry-run compilation is performed to validate the output.
