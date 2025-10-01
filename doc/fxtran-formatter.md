The purpose of `fxtran-formatter` is to reformat some statements.

For now we reformat only the following statements (these happen to be the
most sensitive to merge process) :

- `CALL` statements.
- `SUBROUTINE` statements.
- `ASSOCIATE` statements.
# NAME

[fxtran-formatter](../bin/fxtran-formatter)

# SYNOPSIS

    $ ls -l apl_arpege.F90
    -rw-r--r-- 1 marguina algo 48695 oct.   1 14:45 apl_arpege.F90
    $ fxtran-formatter apl_arpege.F90 
    $ ls -l apl_arpege.F90
    -rw-r--r-- 1 marguina algo 47501 oct.   1 14:52 apl_arpege.F90

# DESCRIPTION

The purpose of `fxtran-formatter` is to reformat some statements.

For now we reformat only the following statements (these happen to be the
most sensitive to merge process) :

- `CALL` statements.
- `SUBROUTINE` statements.
- `ASSOCIATE` statements.

# AUTHOR

philippe.marguinaud@meteo.fr

# SEE ALSO

[Fxtran::Formatter](Fxtran%3A%3AFormatter.md), [fxtran-mergetool](fxtran-mergetool.md)

# COPYRIGHT

Meteo-France 2025
