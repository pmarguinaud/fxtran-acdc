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

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
