# NAME

[Fxtran::Finder::Include](../lib/Fxtran/Finder/Include.pm)

# DESCRIPTION

Finder implementation that searches for files through a list of include
directories, similar to a C preprocessor include path. Directories are passed
via the `I` constructor argument (with or without a leading `-I` prefix).
The current directory `.` is always prepended to the search path. The first
directory that contains the requested file wins.

# FUNCTIONS

## new

Constructor. Accepts an `I` array-ref of include directories (each entry may
optionally carry a leading `-I` prefix, which is stripped). The current
directory `.` is always prepended to the resulting search path.

## resolve

Searches each directory in the include path (`I` list) in order and returns
the first path `I/file` for which the file actually exists on disk. The
`file` argument is passed as a named parameter. Returns undef if the file is
not found in any of the include directories.

# SEE ALSO

[Fxtran::Finder](Fxtran%3A%3AFinder.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
