# NAME

[Fxtran::Directive](../lib/Fxtran/Directive.pm)

# DESCRIPTION

This module provides functions to parse ACDC directives.

# FUNCTIONS

## parseDirectives

Parse ACDC directives, such as:

    !$ACDC PARALLEL {

    !$ACDC }

and:

    !$ACDC singlecolumn

## openmpToACDC

Convert `OpenMP` directives to ACDC parallel sections. The document has to be parsed
with `OpenMP` directives parsing enabled.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
