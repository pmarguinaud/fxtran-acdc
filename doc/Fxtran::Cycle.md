# NAME

[Fxtran::Cycle](../lib/Fxtran/Cycle.pm)

# DESCRIPTION

The purpose of this module is to apply simplifications
(ie set some expressions to constant values and simplify
the code accordingly), these simplifications being cycle 
dependent.

The `Fxtran::Cycle::simplify` will invoke the appropriate
`Fxtran::CycleNN::simplify` method, using the cycle value.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
