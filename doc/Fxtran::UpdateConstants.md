# NAME

[Fxtran::UpdateConstants](../lib/Fxtran/UpdateConstants.pm)

# DESCRIPTION

Inserts update calls for derived-type arguments after they have been modified
inside a subroutine.  For each INOUT dummy argument whose type belongs to a
configurable list, the module tracks every assignment or INOUT call-site that
could mutate the object and injects a conditional `CALL UPDATE(...)` after
each such statement, guarded by a LOGICAL flag.  The corresponding utility
module (`USE UTIL_<TYPE`\_MOD>) is added to the use-part automatically.

# FUNCTIONS

## getActualArgumentIntent

Look up the INTENT of a specific actual argument in a call statement by
fetching and parsing the callee's interface file, then matching the argument
by position or keyword name.

## processSingleRoutine

For each INOUT dummy argument of a tracked derived type, inject a `LLDO_N`
flag, set it to `.TRUE.` after every mutation, and append a conditional
`CALL UPDATE(N, LDCOMPONENT=.TRUE.)` block at the end of the executable
section, together with the corresponding `USE UTIL_<TYPE`\_MOD> statement.

# SEE ALSO

[Fxtran::Decl](Fxtran%3A%3ADecl.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
