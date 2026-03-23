# NAME

[Fxtran::Finder::Pack](../lib/Fxtran/Finder/Pack.pm)

# DESCRIPTION

Finder implementation for gmkpack-style source packs. A pack is a directory
tree whose layout is described by a `.gmkview` file listing the views in
priority order. All `.F90` and `.h` files across every view are indexed by
basename. The index is cached in a `.scan.pl` file inside the pack directory
so that subsequent runs do not need to re-scan the tree. The local (first)
view is always scanned afresh to pick up any recent edits.

# FUNCTIONS

## scanView

Populates (or updates) a scan hash-ref with the `.F90` and `.h` files found
under `pack/src/view/`. Each entry maps the file's basename to its absolute
path. Called internally by `scanpack` for each view in the pack.

## scanpack

Builds the full basename-to-path index for the gmkpack pack. Reads the view
list from `.gmkview`, treats the first entry as the local (mutable) view, and
reverses the remaining views so that higher-priority views overwrite
lower-priority ones in the index. Non-local views are loaded from a
`.scan.pl` cache file if it exists; otherwise they are scanned and the cache
is written. The local view is always rescanned to capture recent edits. Stores
the result in `$self->{scan}`.

## new

Constructor. Calls the parent constructor, then resolves the `pack` attribute
to an absolute path (defaulting to the current directory `.` if not
provided).

## resolve

Resolves a filename (`file` named argument) to its absolute path within the
pack. Triggers `scanpack` on the first call to populate the index. Returns
the path if found in the index, or undef otherwise.
