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
