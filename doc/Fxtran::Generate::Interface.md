# NAME

[Fxtran::Generate::Interface](../lib/Fxtran/Generate/Interface.pm)

# DESCRIPTION

Generate interface block for different transformation methods.

# AUTHOR

philippe.marguinaud@meteo.fr

## interface

Generate the interface block for a given transformation method.  The
function writes the source document to a temporary F90 file prefixed with
the appropriate `!$ACDC` directive, invokes `fxtran-f90` in dry-run mode
to perform the transformation, then parses the resulting file, makes it
canonic, strips all assignment statements, and returns the text of the
interface block body.

# SEE ALSO

[Fxtran::Generate](Fxtran%3A%3AGenerate.md), [Fxtran::Interface](Fxtran%3A%3AInterface.md)

# COPYRIGHT

Meteo-France 2025
