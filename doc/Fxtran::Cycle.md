# NAME

[Fxtran::Cycle](../lib/Fxtran/Cycle.pm)

# DESCRIPTION

The purpose of this module is to apply simplifications
(ie set some expressions to constant values and simplify
the code accordingly), these simplifications being cycle 
dependent.

The `Fxtran::Cycle::simplify` will invoke the appropriate
`Fxtran::CycleNN::simplify` method, using the cycle value.

## simplify

Dispatch cycle-dependent simplifications to the appropriate
`Fxtran::CycleNN` module, where `NN` is the numeric value supplied via
the `cycle` option.  The target module is loaded dynamically with `use`
and its own `simplify` class method is then called with the same
arguments.  Returns immediately if no `cycle` option is provided.

# SEE ALSO

[Fxtran::Cycle49](Fxtran%3A%3ACycle49.md), [Fxtran::Cycle50](Fxtran%3A%3ACycle50.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
