# NAME

[Fxtran::Finder::Files](../lib/Fxtran/Finder/Files.pm)

# DESCRIPTION

Finder implementation backed by an explicit list of files. At construction time
the list is indexed by basename so that `resolve` can look up any file by name
in O(1). Used when the caller supplies a `files` argument to
["new" in Fxtran::Finder](Fxtran%3A%3AFinder#new.md).

# FUNCTIONS
