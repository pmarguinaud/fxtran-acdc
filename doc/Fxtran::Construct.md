# NAME

[Fxtran::Construct](../lib/Fxtran/Construct.pm)

# DESCRIPTION

This module provides functions whose purpose is construct reorganization.

# FUNCTIONS

## changeIfStatementsInIfConstructs

Change statements such as:

    IF (LLCOND) CALL SUBR (...)

into:

    IF (LLCOND) THEN
      CALL SUBR (...)
    ENDIF

## simplify

Constant-fold a single expression node `$e1` within its parent expression.
Handles arithmetic (`+`), comparison (`==` / `/=`), and logical (`.AND.`,
`.OR.`, `.NOT.`) operators with literal operands, and removes redundant
parentheses. Recursively simplifies the result if it is a literal.

## apply

Replace expressions with constants values, and simplify the code (ie remove
some `IF` blocks) when possible.

For instance, a block such as:

    IF (LLCOND1) THEN
      [ list of statements 1 ]
    ELSEIF (LLCOND2) THEN
      [ list of statements 2 ]
    ELSE
      [ list of statements 3 ]
    ENDIF

would become, after forcing `LLCOND2` to `.FALSE.`.

    IF (LLCOND1) THEN
      [ list of statements 1 ]
    ELSE
      [ list of statements 3 ]
    ENDIF

This is achieved by calling `apply` like this:

    &Fxtran::Construct::apply 
    (
      $d,                                 # section to be reorganized
      '//named-E[string(N)="LLCOND2"]',   # XPath expression for expression matching
      &e ('.FALSE.),                      # expression to replace matching expressions with
    );

## simplifyIfConstructs

Remove dead branches from `IF` constructs whose condition has been reduced to
a literal `.TRUE.` or `.FALSE.`. When a single-block construct with a
`.TRUE.` condition remains, the surrounding `IF`/`ENDIF` markers are
removed and the body is inlined.

## removeEmptyDoConstructs

Remove `DO` constructs that contain no statements other than the `DO` and
`ENDDO` delimiters. Empty `IF` constructs nested inside each `DO` are
removed first.

## removeEmptyIfConstructs

Remove `IF` constructs where every block contains only its delimiter
statement (i.e. there are no executable statements inside any branch).

## removeEmptyConstructs

Convenience wrapper that removes both empty `IF` constructs and empty `DO`
constructs from the given document or program unit.

# SEE ALSO

[Fxtran::Decl](Fxtran%3A%3ADecl.md), [Fxtran::Loop](Fxtran%3A%3ALoop.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
