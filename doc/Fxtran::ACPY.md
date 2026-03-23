# NAME

[Fxtran::ACPY](../lib/Fxtran/ACPY.pm)

# DESCRIPTION

This module provides functions to transform array assignments 
into calls to functions which will perform the assignements.

This is meant to avoid the compiler makeing assumptions about
aliasing, which leads to very conservative optimizations and
memory allocations.

# EXAMPLE

    X (:,:) = Y (:,:)

is transformed into:

    CALL FXTRAN_ACDC_ARRAY_COPY (X, Y)

or, even better (for the NVHPC compiler):

    CALL FXTRAN_ACDC_ARRAY_COPY (X, SIZE (X (:,:), 1), SIZE (X (:,:), 2), &
                               & Y, SIZE (Y (:,:), 1), SIZE (Y (:,:), 2))

## useAcpy

Transform array-to-array assignment statements into calls to
`FXTRAN_ACDC_ARRAY_COPY`, passing the NPROMA loop variable and both
array expressions as arguments.  Only assignments where both the
left-hand side and the right-hand side carry section subscripts with an
unknown lower-bound (`?`) are rewritten.

## useBcpy

Like `useAcpy`, but generates a more explicit call to
`FXTRAN_ACDC_ARRAY_COPY` that also passes each dimension size via
`SIZE()` calls.  This variant is intended for the NVHPC compiler, which
benefits from the explicit dimension information to avoid overly
conservative aliasing assumptions.

# SEE ALSO

[Fxtran::ManyBlocks](Fxtran%3A%3AManyBlocks.md), [Fxtran::SingleBlock](Fxtran%3A%3ASingleBlock.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
