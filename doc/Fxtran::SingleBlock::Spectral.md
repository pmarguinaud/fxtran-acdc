# NAME

[Fxtran::SingleBlock::Spectral](../lib/Fxtran/SingleBlock/Spectral.pm)

# DESCRIPTION

The purpose of this module is to apply the singleblock transform to spectral (horizontal diffusion mainly)
routines.

We rely on the `Fxtran::SingleBlock` method and inherit from this class. 

The differences with `Fxtran::SingleBlock` are:

- Most arrays are dimensioned with `NFLEVL` and `NSPEC2`, or `NSPEC2` alone for `PSPSP` (surface pressuse):

        REAL :: PSPDIV (NFLEVG, NSPEC2)
        REAL :: PSPSP (NSPEC2)

- `NPROMA` is `NSPEC` (looks weird, but there is no other alternative).
- We overload the `makeParallel` method, so that loops over levels and wavenumbers be collapsed into a single
loop when possible.

# AUTHOR

philippe.marguinaud@meteo.fr

# SEE ALSO

[Fxtran::SingleBlock](Fxtran%3A%3ASingleBlock.md)

# COPYRIGHT

Meteo-France 2025
