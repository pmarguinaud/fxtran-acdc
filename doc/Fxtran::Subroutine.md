# NAME

[Fxtran::Subroutine](../lib/Fxtran/Subroutine.pm)

# DESCRIPTION

Utilities for manipulating Fortran SUBROUTINE program units in a parsed
document.  `addSuffix` appends a string to the subroutine name in both the
SUBROUTINE and END SUBROUTINE statements as well as in any DR\_HOOK call
strings.  `rename` applies a user-supplied transformation function to the
subroutine name and updates DR\_HOOK strings accordingly.  `getInterface`
locates and parses the interface block for a named subroutine.

# FUNCTIONS
