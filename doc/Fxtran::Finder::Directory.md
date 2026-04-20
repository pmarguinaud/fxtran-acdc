# NAME

[Fxtran::Finder::Directory](../lib/Fxtran/Finder/Directory.pm)

# DESCRIPTION

Finder implementation that resolves all source files relative to a single
directory. The directory is stored in the `directory` attribute of the object.
This is the simplest possible finder strategy.

# FUNCTIONS

## resolve

Resolves a filename (passed as the `file` named argument) by simply
prepending the configured `directory` attribute. Returns the resulting path
string; no existence check is performed.

# SEE ALSO

[Fxtran::Finder](Fxtran%3A%3AFinder.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
