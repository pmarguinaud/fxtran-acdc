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

## addSuffix

Appends `$suffix` to the subroutine name in both the SUBROUTINE and END
SUBROUTINE statements of the program unit `$pu`, and updates any DR\_HOOK
call string literals to include the same suffix.

## rename

Renames a subroutine by applying the user-supplied transformation function
`$sub` to the current name and updating the SUBROUTINE, END SUBROUTINE, and
DR\_HOOK string nodes accordingly.

## getInterface

Locates the interface file for the named subroutine via the `$find` helper,
reads it, and returns the parsed program-unit or interface-construct node.
Dies if the interface file cannot be found or opened.
