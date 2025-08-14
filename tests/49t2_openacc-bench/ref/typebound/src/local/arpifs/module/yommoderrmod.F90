MODULE YOMMODERRMOD

!$ACDC methods 


!     Purpose.
!     --------
!       Controls for model error in the model

!     Author.
!     -------
!       M. Chrust

!     Modifications.
!     --------------
!       Original    03-Jan-2020
! ------------------------------------------------------------------

USE PARKIND1,ONLY : JPIM,JPRB

IMPLICIT NONE

SAVE

!     --------------------------------------------------------------------------------
!     Model error configuration
!     --------------------------------------------------------------------------------

TYPE :: TMODERR

  LOGICAL                         :: LSCALERR      =.FALSE. ! Scale model error
  LOGICAL                         :: LTAPERERR     =.FALSE. ! Taper model error
  INTEGER(KIND=JPIM)              :: NTYPE_MODERR  =0       ! Type of model error
                                                            ! 0 - No model error
                                                            ! 1 - Full state
                                                            ! 2 - Forcing (constant or periodic)
                                                            ! 3 - Bias-like error
  INTEGER(KIND=JPIM)              :: NSTEP_MODERR  =0       ! Number of intervals  in model error
  INTEGER(KIND=JPIM)              :: NORDER_MODERR =0       ! Order of model error forcing
  INTEGER(KIND=JPIM)              :: NPRTMODERR    =0       ! Printing level for diagnostics
  INTEGER(KIND=JPIM)              :: TAPERERR_INI  =0       ! Start of tapering
  INTEGER(KIND=JPIM)              :: TAPERERR_END  =0       ! End of tapering
  REAL(KIND=JPRB)                 :: TSTEP_ERR     =0.0     ! Forcing time-step
  INTEGER(KIND=JPIM), ALLOCATABLE :: MSTEPERR(:)            ! Index model error steps
  REAL(KIND=JPRB), ALLOCATABLE    :: TSTEPERR(:)            ! Taper coefficient
  REAL(KIND=JPRB), ALLOCATABLE    :: FSTEPERR(:,:)          ! Coef. periodic forcing

CONTAINS

  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION

PROCEDURE :: ACDC_COPY => ACDC_COPY_TMODERR
PROCEDURE :: ACDC_CRC64 => ACDC_CRC64_TMODERR
PROCEDURE :: ACDC_HOST => ACDC_HOST_TMODERR
PROCEDURE :: ACDC_LOAD => ACDC_LOAD_TMODERR
PROCEDURE :: ACDC_SAVE => ACDC_SAVE_TMODERR
PROCEDURE :: ACDC_SIZE => ACDC_SIZE_TMODERR
PROCEDURE :: ACDC_WIPE => ACDC_WIPE_TMODERR
END TYPE TMODERR

!     --------------------------------------------------------------------------------
INTERFACE

MODULE SUBROUTINE ACDC_COPY_TMODERR (SELF, LDCREATED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TMODERR), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED, LDFIELDAPI
END SUBROUTINE

MODULE SUBROUTINE ACDC_CRC64_TMODERR (SELF, KLUN, CDPATH)
USE FXTRAN_ACDC_CRC64_INTRINSIC_MOD, ONLY : FCRC64 => FXTRAN_ACDC_CRC64
IMPLICIT NONE
CLASS (TMODERR), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
CHARACTER(LEN=*), INTENT (IN) :: CDPATH
END SUBROUTINE

MODULE SUBROUTINE ACDC_HOST_TMODERR (SELF)

IMPLICIT NONE
CLASS (TMODERR), TARGET :: SELF
END SUBROUTINE

MODULE SUBROUTINE ACDC_LOAD_TMODERR (SELF, KLUN)
USE PARKIND1, ONLY : JPRD
IMPLICIT NONE
CLASS (TMODERR), INTENT (OUT), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE SUBROUTINE ACDC_SAVE_TMODERR (SELF, KLUN)

IMPLICIT NONE
CLASS (TMODERR), INTENT (IN), TARGET :: SELF
INTEGER, INTENT (IN) :: KLUN
END SUBROUTINE

MODULE FUNCTION ACDC_SIZE_TMODERR (SELF, CDPATH, LDPRINT) RESULT (KSIZE)

IMPLICIT NONE
CLASS (TMODERR),     INTENT (IN), TARGET :: SELF
CHARACTER(LEN=*), INTENT (IN), OPTIONAL :: CDPATH
LOGICAL,          INTENT (IN), OPTIONAL :: LDPRINT
INTEGER*8 :: KSIZE
END FUNCTION

MODULE SUBROUTINE ACDC_WIPE_TMODERR (SELF, LDDELETED, LDFIELDAPI)

IMPLICIT NONE
CLASS (TMODERR), INTENT (IN), TARGET :: SELF
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED, LDFIELDAPI
END SUBROUTINE


END INTERFACE

CONTAINS

SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
  IMPLICIT NONE
  CLASS(TMODERR), INTENT(IN) :: SELF
  INTEGER       , INTENT(IN) :: KDEPTH
  INTEGER       , INTENT(IN) :: KOUTNO

  INTEGER :: IDEPTHLOC

  IDEPTHLOC = KDEPTH+2

  WRITE(KOUTNO,*) REPEAT(' ',KDEPTH   ) // 'model%yrml_gconf%tmoderr : '
  WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'NTYPE_MODERR  = ', SELF%NTYPE_MODERR
  WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'NORDER_MODERR = ', SELF%NORDER_MODERR
  WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'NPRTMODERR    = ', SELF%NPRTMODERR
  WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'LTAPERERR     = ', SELF%LTAPERERR
  WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'TAPERERR_INI  = ', SELF%TAPERERR_INI
  WRITE(KOUTNO,*) REPEAT(' ',IDEPTHLOC) // 'TAPERERR_END  = ', SELF%TAPERERR_END

  WRITE(KOUTNO,*) ''

END SUBROUTINE PRINT_CONFIGURATION

!     --------------------------------------------------------------------------------

END MODULE YOMMODERRMOD
