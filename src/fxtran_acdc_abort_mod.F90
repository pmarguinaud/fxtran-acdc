MODULE FXTRAN_ACDC_ABORT_MOD

#include "fxtran_acdc_config.h"

!
! Copyright 2025 Meteo-France
! All rights reserved
! philippe.marguinaud@meteo.fr
!

CONTAINS

SUBROUTINE FXTRAN_ACDC_ABORT (CDMESS)

CHARACTER (LEN=*) :: CDMESS

FXTRAN_ACDC_ROUTINE_SEQ

PRINT *, " FXTRAN_ACDC_ABORT "
PRINT *, CDMESS
STOP

END SUBROUTINE

END MODULE
