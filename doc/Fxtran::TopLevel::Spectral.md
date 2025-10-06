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

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
