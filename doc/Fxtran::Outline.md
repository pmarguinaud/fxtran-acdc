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
