# NAME

[Fxtran::Cycle49](../lib/Fxtran/Cycle49.pm)

# DESCRIPTION

Simplification rules specific to IFS cycle 49.  Provides a `simplify` method
that rewrites a set of named Boolean or string variables to their known
compile-time values for that cycle, removing dead code branches.  Additional
variables can be overridden at run time via the `set-variables` option.

# FUNCTIONS
