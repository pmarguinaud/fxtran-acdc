# NAME

[Fxtran::Decl](../lib/Fxtran/Decl.pm)

# DESCRIPTION

This module provides functions for manipulating data declaration.

# FUNCTIONS

## forceSingleDecl

This function transforms statements such as:

    REAL :: X, Y, Z

into:

    REAL :: X
    REAL :: Y 
    REAL :: Z

## declare

This routine takes a program unit as argument, and statements to add
in the declaration part. These statements can be strings or XML nodes. 

Only statements declaring a single entity are accepted. Statements
redeclaring already declared entities are ignored.

## use

This routine takes a program unit as argument, and use statements to 
add in the use part. These statements can be either strings or XML nodes.

Statements importing existing modules are ignored.

## include

This method takes a program unit as argument, and include statements to
add in the declaration part.

Already included files are not included again.

## addAttributes

Add attributes to a declaration statement. For instance:

    REAL :: X

can be added a POINTER attribute:

    REAL, POINTER :: X

## removeAttributes

Remove attributes from a declaration statement. For instance:

    REAL, POINTER :: X

can be removed its POINTER attribute:

    REAL :: X

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
