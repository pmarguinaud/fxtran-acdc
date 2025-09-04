# NAME

[Fxtran::Inline](../lib/Fxtran/Inline.pm)

# DESCRIPTION

The purpose of this module is to provide functions to inline subroutine calls.

# FUNCTIONS

## inlineContainedSubroutines

Inline all routines from the CONTAINS section. Sort these
routines: inline first routines which are not called by
any other CONTAINed subroutine.

Remove the CONTAINS statement and the CONTAINed subroutines.

## inlineExternalSubroutine

Inline external subroutine. Suffix inlined routine variables with inlined routine name.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
