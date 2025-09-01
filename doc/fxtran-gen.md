`fxtran-gen` is a `click` frontend to `Fxtran::Generate` and other generation modules. It
is invoked by `fxtran-f90` to transform FORTRAN source code.

The first argument of fxtran-gen is the name of the generation module (this should be 
a clickable module), but this argument is optional and defaults to `Fxtran::Generate`. Then
comes the method to be invoked, followed by its options and arguments.

See `Fxtran::Generate` for the details of methods and options accepted by `fxtran-gen`.
# NAME

[fxtran-gen](../bin/fxtran-gen)

# SYNOPSIS

    $ fxtran-gen Fxtran::Generate singlecolumn [.. options for singlecolumn ..] file.F90

or (generation module defaults to `Fxtran::Generate`):

    $ fxtran-gen singlecolumn [.. options for singlecolumn ..] file.F90

# DESCRIPTION

`fxtran-gen` is a `click` frontend to `Fxtran::Generate` and other generation modules. It
is invoked by `fxtran-f90` to transform FORTRAN source code.

The first argument of fxtran-gen is the name of the generation module (this should be 
a clickable module), but this argument is optional and defaults to `Fxtran::Generate`. Then
comes the method to be invoked, followed by its options and arguments.

See `Fxtran::Generate` for the details of methods and options accepted by `fxtran-gen`.

# SEE ALSO

[click](click.md), [Fxtran::Generate](Fxtran%3A%3AGenerate.md), [fxtran-f90](fxtran-f90.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
