# NAME

[Fxtran::Cycle50](../lib/Fxtran/Cycle50.pm)

# DESCRIPTION

Simplification rules specific to IFS cycle 50.  Provides a `simplify` method
that rewrites a set of named Boolean or string variables to their known
compile-time values for that cycle.  Compared to cycle 49 it includes
additional variables and also renames YDGEO to YDGEOMETRY and removes calls to
GET\_HALO\_PHY.  Additional variables can be overridden at run time via the
`set-variables` option.

# FUNCTIONS
