The purpose of `fxtran-fix` is to trap compiler errors and give the user 
a chance to debug them, without interrupting the compiling process (cmake
or gmkpack).

When a compilation error occurs, `fxtran-fix` will catch it, if the environment
variable `FXTRANFIX` is not null, and create a new shell (via xterm or screen)
which the user can use to find and fix the issue.

In this shell session, two commands are provided:

- e

    Edit the file currently being compiled.

- r

    Rerun the compiler command.

When the user is done with editing/debugging, he has to exit the shell; `fxtran-fix` 
will then attempt to compile the code once more and return control to the 
build system.
# NAME

[fxtran-fix](../bin/fxtran-fix)

# SYNOPSIS

    $ FXTRANFIX=1 fxtran-fix --type xterm -- fxtran-f90 ...

# DESCRIPTION

The purpose of `fxtran-fix` is to trap compiler errors and give the user 
a chance to debug them, without interrupting the compiling process (cmake
or gmkpack).

When a compilation error occurs, `fxtran-fix` will catch it, if the environment
variable `FXTRANFIX` is not null, and create a new shell (via xterm or screen)
which the user can use to find and fix the issue.

In this shell session, two commands are provided:

- e

    Edit the file currently being compiled.

- r

    Rerun the compiler command.

When the user is done with editing/debugging, he has to exit the shell; `fxtran-fix` 
will then attempt to compile the code once more and return control to the 
build system.

# SEE ALSO

[fxtran-f90](fxtran-f90.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
