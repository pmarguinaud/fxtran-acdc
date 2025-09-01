# NAME

[Fxtran::Call](../lib/Fxtran/Call.pm)

# DESCRIPTION

This package provides functions to work on call statements. 

# FUNCTIONS

## addSuffix

The `addSuffix` function
works on a code section, and adds a suffix to the procedure designator of all
call statements.

Interface includes and interfaces provided by modules are updated accordingly.

Some procedure designators may be excluded from the processing, using the `match`
callback, passed as argument. 

Contained subroutines may be excluded from the processing if the argument `contained`
is present.

## getArgumentIntent

This function returns the argument intent of an expression passed
as argument to a call statement.

We rely on the `$find` argument of `getArgumentIntent` to find
the subroutine interface and retrieve the dummy argument intent.

# grokIntent

This function finds the intent of an expression (IN or INOUT) in any statement.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
