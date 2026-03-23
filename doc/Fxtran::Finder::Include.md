# NAME

[Fxtran::Finder::Include](../lib/Fxtran/Finder/Include.pm)

# DESCRIPTION

Finder implementation that searches for files through a list of include
directories, similar to a C preprocessor include path. Directories are passed
via the `I` constructor argument (with or without a leading `-I` prefix).
The current directory `.` is always prepended to the search path. The first
directory that contains the requested file wins.

# FUNCTIONS
