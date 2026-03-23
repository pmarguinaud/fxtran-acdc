# NAME

[Fxtran::SemiImplicit](../lib/Fxtran/SemiImplicit.pm)

# DESCRIPTION

This transformation relies mostly on the `Fxtran::SingleBlock` transformation.

After applying the singleblock transformation, horizontal sections 
(marked with `!$ACDC HORIZONTAL`) are
scanned and call to routines implementing horizontal operators are added
the `LDACC=LDACC` optional argument.

# SEE ALSO

[Fxtran::SingleBlock](Fxtran%3A%3ASingleBlock.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

## processSingleRoutine

Apply the single-block transformation to a program unit and then augment
it for the semi-implicit scheme.  First delegates to
`Fxtran::SingleBlock::processSingleRoutine` using the suffix
`_SINGLEBLOCK`, then scans every `horizontal-section` in the transformed
unit and appends the `LDACC=LDACC` named argument to every `CALL`
statement found therein, so that horizontal operators receive the
accumulation flag.
