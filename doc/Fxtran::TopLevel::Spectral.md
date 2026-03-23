# NAME

[Fxtran::TopLevel::Spectral](../lib/Fxtran/TopLevel/Spectral.pm)

# DESCRIPTION

The purpose of this module is to transform high level routines (such as spcm.F90), to 
enable OpenACC.

# EXAMPLE

A sequence such as:

    !$ACDC PARALLEL {

      ZSPSPG   => GET_HOST_DATA_RDWR (YLSPG )
      ZSPVORG  => GET_HOST_DATA_RDWR (YLVORG)
      ...
     
      CALL SPCSI(...)
    
    !$ACDC }

is transformed into:

    IF (FXTRAN_ACDC_LPARALLELMETHOD ('OPENACCSINGLEBLOCK', 'SPCMSI:0')) THEN
      
      ZSPSPG=>GET_DEVICE_DATA_RDWR (YLSPG)
      ZSPVORG=>GET_DEVICE_DATA_RDWR (YLVORG)
      ...

      CALL SPCSI_SINGLEBLOCK (..., LDACC=.TRUE.)
      
    ELSEIF (FXTRAN_ACDC_LPARALLELMETHOD ('SINGLEBLOCK', 'SPCMSI:0')) THEN
      
      ZSPSPG=>GET_HOST_DATA_RDWR (YLSPG)
      ZSPVORG=>GET_HOST_DATA_RDWR (YLVORG)
      ...

      CALL SPCSI_SINGLEBLOCK (..., LDACC=.FALSE.)
      
    ELSE

      ZSPSPG=>GET_HOST_DATA_RDWR (YLSPG)
      ZSPVORG=>GET_HOST_DATA_RDWR (YLVORG)
      ...

      CALL SPCSI (...)

    ENDIF

    IF (FXTRAN_ACDC_LSYNCHOST ('SPCMSI:0')) THEN
      ZSPSPG=>GET_HOST_DATA_RDWR (YLSPG)
      ZSPVORG=>GET_HOST_DATA_RDWR (YLVORG)
      ...

    ENDIF

# SEE ALSO

[Fxtran::TopLevel](Fxtran%3A%3ATopLevel.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025

## processSingleRoutine

Dispatch to `processSingleRoutineMethod` for the spectral top-level transformation.

## renameProc

Rename a called procedure by appending the singleblock suffix when its name matches the `SP*` or `ESP*` pattern.

## processSingleRoutineMethod

Transform all `ACDC PARALLEL` sections in a top-level spectral routine into guarded dispatch blocks supporting OpenACC, singleblock, and fallback execution paths.
