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

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
