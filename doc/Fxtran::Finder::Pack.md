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
