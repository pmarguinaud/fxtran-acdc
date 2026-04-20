# NAME

[Fxtran::Style::ACRANEB2](../lib/Fxtran/Style/ACRANEB2.pm)

# DESCRIPTION

Style class for the ACRANEB2 radiation scheme. This class derives from
`Fxtran::Style::MFPHYS` and handles the specific naming conventions used
in the ACRANEB2 code, where the horizontal loop iterator is `JN` and the
loop size argument is `KJN` instead of the usual `JLON`/`KLON`.

The `preProcessForOpenACC` method normalises ACRANEB2 code before OpenACC
parallelisation: it renames `KJN` to `KLON`, removes the `JN` loops,
and replaces the local `IIDIA`/`IFDIA` bounds variables with the
standard `KIDIA`/`KFDIA` dummy arguments.

The `matchDocument` method identifies ACRANEB2 source files by the
presence of both `KLON` and `KJN` dummy arguments together with either
a `KIIDIA` argument or an `IIDIA` local variable declaration.

# SEE ALSO

[Fxtran::Style](Fxtran%3A%3AStyle.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
