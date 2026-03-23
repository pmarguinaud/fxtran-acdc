# NAME

[Fxtran::Finder::Files](../lib/Fxtran/Finder/Files.pm)

# DESCRIPTION

Finder implementation backed by an explicit list of files. At construction time
the list is indexed by basename so that `resolve` can look up any file by name
in O(1). Used when the caller supplies a `files` argument to
["new" in Fxtran::Finder](Fxtran%3A%3AFinder#new.md).

# FUNCTIONS

## new

Constructor. Accepts a `files` array-ref of relative file paths and a
`base` directory prefix. Indexes all provided files by their basename so
that `resolve` can perform O(1) lookups.

## resolve

Looks up the filename given by the `file` named argument in the basename
index built during construction. Returns the full path (`base`/relative) if
found, or undef otherwise.
