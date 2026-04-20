# NAME

[Fxtran::IO::spectral](../lib/Fxtran/IO/spectral.pm)

# DESCRIPTION

FieldAPI class policy module for spectral derived types.  Provides
`skip` and `getFieldAPIMember` methods used by `Fxtran::IO` when
generating I/O code.  Pointer components that do not start with `F_`
are skipped, as are components matching `SP\dD` (spectral
dimensioned arrays).

# SEE ALSO

[Fxtran::IO](Fxtran%3A%3AIO.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
