# NAME

[Fxtran::TopLevel](../lib/Fxtran/TopLevel.pm)

# DESCRIPTION

The purpose of this module is to transform top-level routines such as `cpg_drv.F90` so that
generated parallel grid-point routines be called. 

This module does differents things:

## switch

There is a `--switch` option to the transformation. The following sequence of statements:

    SUBROUTINE CPG_DRV (...)

    !$ACDC toplevel --switch LLPARALLEL

    ...

    LLPARALLEL = .FALSE.

is transformed into:

    LLPARALLEL=FXTRAN_ACDC_LPARALLELMETHOD ('PARALLEL','CPG_DRV')

    ...

so that the user can choose to enable the grid-point parallel routines.

## COPY directive

A block delimited by this directive:

    ...

    !$ACDC COPY, IF=LLPARALLEL {
    
    LLPERSISTENT = LLPARALLEL
    
    CALL YLCPG_DYN0%INIT (..., PERSISTENT=LLPERSISTENT)
    
    CALL YLCPG_PHY0%INIT (..., PERSISTENT=LLPERSISTENT)

    ...

    !$ACDC }
    

is transformed into:

    CALL YLCPG_DYN0%INIT (..., PERSISTENT=LLPERSISTENT)

    IF (LLPARALLEL) THEN
      CALL ACDC_COPY (YLCPG_DYN0)
    ENDIF

    CALL YLCPG_PHY0%INIT (..., PERSISTENT=LLPERSISTENT)

    IF (LLPARALLEL) THEN
      CALL ACDC_COPY (YLCPG_PHY0)
    ENDIF

    ...

so that scoped data structures be copied on the device.

## PARALLEL directive

A block delimited by the `PARALLEL` directive:

    IF (LLPARALLEL) THEN
    
    !$ACDC PARALLEL {
    
      CALL CPG (...)
    
    !$ACDC }
    
    ELSE
    
    ...
    
    ENDIF

is tranformed into:

    IF (LLPARALLEL) THEN

      CALL YFXTRAN_ACDC_STACK%INIT (YDCPG_OPTS%KLON, YDCPG_OPTS%KFLEVG, YDCPG_OPTS%KGPBLKS)

      CALL CPG_PARALLEL (...)
     
      IF (FXTRAN_ACDC_LSYNCHOST ('CPG_DRV')) THEN
        CALL HOST (YDA_EXTRA)
        CALL HOST (YDA_GFLPC)
        ...
      ENDIF
    ELSE
      ...
    ENDIF

The result is that the parallel version of `cpg.F90` is called, and optionally, involved
Field API backed objects are synchronized on the CPU.

## WIPE directive

A block delimited by this directive:

    !$ACDC WIPE, IF=LLPARALLEL {
    
    ...

    CALL YLCPG_PHY0%FINAL
    CALL YLCPG_DYN0%FINAL
    
    !$ACDC }

is transformed into:

    IF (LLPARALLEL) THEN
      CALL ACDC_WIPE (YLCPG_PHY0)
    ENDIF

    CALL YLCPG_PHY0%FINAL

    IF (LLPARALLEL) THEN
      CALL ACDC_WIPE (YLCPG_DYN0)
    ENDIF

    CALL YLCPG_DYN0%FINAL

So that scoped data structures be removed from the device before being destroyed.

# ROUTINES

This is the list of IAL routines which are instrumented using the toplevel method:

- `cpg_drv.F90`
- `cpglag_drv.F90`
- `lapinea_drv.F90`
- `lapineb_drv.F90`
- `scan2m_ctvtot_drv.F90`
- `scan2m.F90`
- `scan2m_ctvtot_drv.F90`

# AUTHOR

philippe.marguinaud@meteo.fr

# SEE ALSO

[Fxtran::Generate](Fxtran%3A%3AGenerate.md)

# COPYRIGHT

Meteo-France 2025
