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

## makeParallel

Overloaded parallelisation method for spectral routines.  Given a program
unit and a candidate `DO` construct, this method first checks whether all
arrays that are written inside the loop are updated within a `JLEV` loop
over levels.  If some are not, only a simple gang/vector loop over the
spectral wavenumber is inserted.  Otherwise all existing `JLEV` loops
inside the construct are hoisted out, a single enclosing `JLEV` loop is
wrapped around the contents, and a collapsed (level x wavenumber) parallel
loop is generated.

# SEE ALSO

[Fxtran::SingleBlock](Fxtran%3A%3ASingleBlock.md)

# COPYRIGHT

Meteo-France 2025
