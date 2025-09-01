# NAME

[Fxtran::Finder](../lib/Fxtran/Finder.pm)

# DESCRIPTION

This is a factory class for all finder objects; the `new` method
picks the right finder class from the context.

A finder object is responsible for finding files in the project
being compiled. It provides the following methods:

- resolve

    Return a full path name given a filename.

- getInterface

    Return the full path of an interface from a subroutine name.

We currently provide concrete classes for the following environments:

- cmake
- gmkpack
- A simple directory containing all source files.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
