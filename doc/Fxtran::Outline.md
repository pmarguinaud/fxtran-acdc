# NAME

[Fxtran::Outline](../lib/Fxtran/Outline.pm)

# DESCRIPTION

Extracts marked code sections from a Fortran subroutine and turns each section
into a new, standalone subroutine (outlining).  For every section delimited by
an ACDC directive, the module analyses the symbols used in that section,
determines which ones need to be passed as dummy arguments versus which are
purely local, copies the necessary declarations, and rewrites the original
code to call the new subroutine.  Variable names are adjusted to follow the
DOCTOR naming conventions.

# FUNCTIONS

## sortArgs

Sorts a list of variable names so that dummy arguments of the subroutine `$d`
appear first (in their original declaration order), followed by local variables
in alphabetical order.

## renameVariables

Builds a name-mapping hash that renames variables according to DOCTOR Fortran
naming conventions (e.g. `K` prefix becomes `P`, `I` becomes `K`).
Variables already conforming to a target prefix, or marked as local or
parameter, are kept unchanged.  Returns a hash ref from old name to new name.

## outlineSection

Extracts a single ACDC-directive-delimited section from subroutine `$d` into
a new standalone subroutine.  Analyses which symbols must be passed as dummy
arguments versus kept local, copies and adapts declarations, renames variables
to DOCTOR conventions, replaces the section with a CALL statement, and returns
an array ref containing the new subroutine node, the call statement node, and
the include node.

## outline

Finds all ACDC-directive sections in the subroutine parse tree `$d` and
calls `outlineSection` on each one.  Returns a list of `[$subroutine,
$call, $include]` array refs, one per outlined section.

# SEE ALSO

[Fxtran::Generate](Fxtran%3A%3AGenerate.md), [Fxtran::Subroutine](Fxtran%3A%3ASubroutine.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
