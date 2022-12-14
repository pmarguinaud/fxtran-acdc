MODULE YOECND

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

!     -----------------------------------------------------------------

TYPE :: TECND
REAL(KIND=JPRB) :: REPFLM
REAL(KIND=JPRB) :: REPQMI
!----------------------------------------------------------------------------
CONTAINS
   
END TYPE TECND
!============================================================================

!!TYPE(TECND), POINTER :: YRECND => NULL()

!     -----------------------------------------------------------------
!*    CONTROL PARAMETERS FOR MOIST PROCESSES

! REPFLM :  Minimum flux to avoid zero division in ice proportion
!           computations
! REPQMI :  Minimum specific humidity (security within QNEGAT)
!     -----------------------------------------------------------------

CONTAINS



END MODULE YOECND
