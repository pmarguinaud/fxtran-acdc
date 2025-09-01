# NAME

[Fxtran::Dimension](../lib/Fxtran/Dimension.pm)

# DESCRIPTION

This module provides functions for manipulating array dimensions.

# FUNCTIONS

## attachArraySpecToEntity

Transform statements such as:

    REAL, DIMENSION (N) :: X, Y

into:

    REAL :: X (N), Y (N)

## fuseOuterDimensions

This routine takes as argument a program unit, and a hash whose keys are the
symbols to process and the values the number of dimensions to fuse.

For instance:

    {
      Z => 2,
    }

    REAL :: Z (NPROMA, NFLEVG, NDIM)
    
    INTEGER :: JLON, JLEV, JDIM

    DO JDIM = 1, NDIM
      DO JLEV = 1, NFLEVG
        DO JLON = 1, NPROMA
          Z (JLON, JLEV, JDIM) = ...
        ENDDO
      ENDDO
    ENDDO

will yield:

    REAL :: Z (NPROMA, NFLEVG*NDIM)

    DO JDIM = 1, NDIM
      DO JLEV = 1, NFLEVG
        DO JLON = 1, NPROMA
          Z (JLON, 1 + (JLEV-1) + NFLEVG* (JDIM-1)) = ...
        ENDDO
      ENDDO
    ENDDO

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022
