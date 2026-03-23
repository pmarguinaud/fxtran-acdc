# NAME

[Fxtran::Print](../lib/Fxtran/Print.pm)

# DESCRIPTION

The purpose of this module is to provide functions to transform
`PRINT` statements, `WRITE` statements, calls to `ABOR1`
or MesoNH `PRINT_MSG`. These statements cannot be used
on the device, and need to be replaced by a simple `PRINT`
statements and/or a `STOP` statement.

# FUNCTIONS

## useABOR1\_ACC

Replace calls to `ABOR1` by calls to `FXTRAN_ACDC_ABORT`, which can be used 
on the device.

## removeTRIM

Remove `TRIM` function calls from an expression, replacing each `TRIM(x)` with `x`.

## changeWRITEintoPRINT

Change `WRITE` statements (not supported on the device) into `PRINT` statements.

## changePRINT\_MSGintoPRINT

Change calls to `PRINT_MSG` into calls to `ABOR1` or `PRINT` statements.

# SEE ALSO

[Fxtran::Message](Fxtran%3A%3AMessage.md).

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
