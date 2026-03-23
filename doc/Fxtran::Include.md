# NAME

[Fxtran::Include](../lib/Fxtran/Include.pm)

# DESCRIPTION

Manages Fortran INCLUDE directives in a parsed document.  Provides functions
to remove interface-block includes that are no longer referenced, to insert new
include directives into the declaration part of a program unit, and to inline
(load and expand) include files that appear after a CONTAINS statement.

# FUNCTIONS

## removeUnusedIncludes

Removes interface-block include directives from the declaration part that are no
longer referenced by any call statement in the execution part.

## addInclude

Inserts one or more include directives into the declaration part of a program
unit, placing them after any existing include or at the end of the declaration
part.

## loadContainedIncludes

Inlines include files that appear after a CONTAINS statement: each such include
is parsed, canonicalised, and its program units are inserted in place of the
include directive.
