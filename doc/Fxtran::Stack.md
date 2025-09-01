# NAME

[Fxtran::Stack](../lib/Fxtran/Stack.pm)

# DESCRIPTION

This module contains utilities to allocate temporary variables using the fxtran stack
data structures.

# FUNCTIONS

## iniStackSingleBlock

Initialize the fxtran stack object at the top of an OpenACC kernel. This is the version
used for regular single-column kernels.

To be simplified when we enforce the `stack-method` option.

## iniStackManyBlocks

Stack initialization for the manyblocks transformation. Use the number of blocks and the stack offset.

To be simplified when the option `stack-method` is enforced.

## addStackInSection

Add the `YDSTACK` argument to calls to calculation routines. Serial routines (we use a hard-wired list for
now) are excluded. This works on a code section.

## addStack

Add a stack object argument and local variable to the program unit passed as argument. Then 
pass the local stack to the calculation routines, and allocate temporaries within the local
stack frame (wrap their declaration with the `fxtran_acdc_temp` macro and allocate with
stack methods.

    SUBROUTINE ACTKE (..., YDSTACK)

    ...                                              ! dummy arguments declaration

    TYPE (FXTRAN_ACDC_STACK), INTENT (IN) :: YDSTACK ! Stack argument

    TYPE (FXTRAN_ACDC_STACK) :: YLSTACK ! Local stack
    
    fxtran_acdc_temp (REAL (KIND=JPRB), ZTEMP, (KLON, KLEV))  ! Declare temporary with macro

    YLSTACK = YDSTACK          ! local stack frame setup
    
    fxtran_acdc_alloc (ZTEMP)  ! allocate in YLSTACK

    CALL ACTURB (..., YLSTACK) ! pass current stack pointer to ACTURB

# EXAMPLES

You can find examples in all single-column routines; for instance:

[actke.F90](../tests/49t2_openacc-bench/src/main/arpifs/phys_dmn/actke.F90)
/
[actke\_openacc.F90](../tests/49t2_openacc-bench/ref/util/src/local/arpifs/phys_dmn/actke_openacc.F90)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
