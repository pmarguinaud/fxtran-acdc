# NAME

[Fxtran::Style::MESONH](../lib/Fxtran/Style/MESONH.pm)

# DESCRIPTION

Style class for MesoNH physics routines. This class derives directly from
`Fxtran::Style` and defines the naming conventions used in MesoNH code,
where the horizontal loop iterator is `JI` (or `JIJ`), the loop bounds
are `D%NIB` and `D%NIE`, and the loop size (nproma) is `D%NIT` or
`D%NIJT`. The vertical level iterator is `JK`.

The class uses `DIMPHYEX_T` structure variables (`D` and a copy named
`DD`) as custom iterators and provides methods to update them from
`YLCPG_BNDS%KIDIA`/`YLCPG_BNDS%KFDIA` bounds.

The `preProcessForOpenACC` method renames `JIJ` to `JI` and normalises
`IIJB`/`IIJE` bound references to `D%NIJB`/`D%NIJE`.

The `matchDocument` method identifies MesoNH source files by the presence
of a `D` dummy argument declared as `TYPE(DIMPHYEX_T)`, or by a `JI`
or `JIJ` local variable declaration in a routine with a `D` argument.

# SEE ALSO

[Fxtran::Style](Fxtran%3A%3AStyle.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
